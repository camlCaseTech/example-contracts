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

module Caller where

import qualified Data.Map as M
import Lorentz
import Michelson.Typed (BigMap)
import Michelson.Printer
import Prelude (Bool(..), Either, Int, Natural, Text, toText)
import GHC.Generics (Generic)

data Parameter
  = Init InitParams
  | ReceiveCallback CallbackParams
  deriving stock Generic
  deriving anyclass IsoValue

type InitParams = ()
type CallbackParams = Text -- update the message

data Storage =
  Storage
    { message   :: Text
    , callbackContract :: ContractAddr (Either Text (ContractAddr (Either () Text)))
    }
    deriving stock Generic
    deriving anyclass IsoValue

callerContract :: Contract Parameter Storage
callerContract = do
  unpair
  caseT @Parameter
    ( #cInit /-> do
        -- drop the input
        drop; -- [Storage]

        -- get this contract address
        self; -- [Contract 'p, Storage]
        right;
        dip (getField #callbackContract #
             balance -- get the tezos from the contract
            )
        transferTokens; -- [Operations, Storage]

        -- set the storage message
        swap;       -- [Storage, Operations]
        push ("Called the contract." :: Text)
        setField #message;

        -- combine operations
        swap; nil; swap; cons; pair

    , #cReceiveCallback /-> do
        -- input is message
        setField #message;
        nil; pair
    )

printCallerContract :: Text
printCallerContract = toText $ printTypedContract $ compileLorentz callerContract
