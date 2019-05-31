{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE NoApplicativeDo        #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE OverloadedLabels, ViewPatterns #-}

-- test adding embeded records to Storage
-- and see if we can use the getters and accessors on it

module CallAndCallback where

import qualified Data.Map as M
import Lorentz
import Michelson.Typed (BigMap)
import Michelson.Printer
import Tezos.Core (unsafeMkMutez)
import Prelude (Bool(..), Either, Int, Natural, Text, toText)
import GHC.Generics (Generic)

-- callback

data CallbackParameter
  = Init Text
  | ReceiveCallback (View () Text)
  deriving stock Generic
  deriving anyclass IsoValue

data CallbackStorage =
  CallbackStorage
    { message   :: Text
    }
    deriving stock Generic
    deriving anyclass IsoValue

callbackContract :: Contract CallbackParameter CallbackStorage
callbackContract = do
  unpair
  caseT @CallbackParameter
    ( #cInit /-> do
        stackType @[Text, CallbackStorage]
        setField #message; nil; pair
    , #cReceiveCallback /-> view_ $ do
        cdr; toField #message
    )

printCallbackContract :: Text
printCallbackContract = toText $ printTypedContract $ compileLorentz callbackContract

-- caller

data CallerParameter
  = RequestMessage ()
--  = RequestMessage (View () CallbackParameter)
  | MessageResponse Text
  deriving stock Generic
  deriving anyclass IsoValue

data CallerStorage =
  CallerStorage
    { response  :: Text
--    , contract  :: (View () CallbackParameter)
    , contract :: ContractAddr (Either Text (ContractAddr (Either () Text)))
--    , contract  :: ContractAddr CallbackParameter
    }
    deriving stock Generic
    deriving anyclass IsoValue

callerContract :: Contract CallerParameter CallerStorage
callerContract = do
  unpair
  caseT @CallerParameter
    -- ( #cRequestMessage /-> do
    --     drop
    --     getField #contract
    --     view_ $ do
    --       cdr
    --       toField #param
    ( #cRequestMessage /-> do
        drop

        -- call the contract
        self
        right
        dip ( getField #contract # balance )
        transferTokens

        -- set the storage message
        swap       -- [Storage, Operations]
        push ("Called the contract." :: Text)
        setField #response

        -- combine operations
        swap; nil; swap; cons; pair

    , #cMessageResponse /-> do
        setField #response
        nil
        pair
    )

printCallerContract :: Text
printCallerContract = toText $ printTypedContract $ compileLorentz callerContract
