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

module Callback where

import qualified Data.Map as M
import Lorentz
import Michelson.Typed (BigMap)
import Michelson.Printer
import Tezos.Core (unsafeMkMutez)
import Prelude (Bool(..), Either, Int, Natural, Text, toText)
import GHC.Generics (Generic)

data Parameter
  = Init InitParams
  | ReceiveCallback CallbackParams
  deriving stock Generic
  deriving anyclass IsoValue

type InitParams = Text
type CallbackParams = ContractAddr (Either () Text)

data Storage =
  Storage
    { message   :: Text
    }
    deriving stock Generic
    deriving anyclass IsoValue

callbackContract :: Contract Parameter Storage
callbackContract = do
  unpair
  caseT @Parameter
    ( #cInit /-> do
        set_ #message; nil; pair
        
    , #cReceiveCallback /-> do
        -- get the message
        dip (dup # access_ #message) -- [Input, Text, Text]
        swap -- [Text, Input, Text]
        -- make a Right Unit
        right
        -- add 0 tezos
        dip (push $ unsafeMkMutez 0)
        -- transfer to initiate the callback
        transferTokens
        nil; swap; cons; pair        
    )

printCallbackContract :: Text
printCallbackContract = toText $ printTypedContract $ compileLorentz callbackContract
