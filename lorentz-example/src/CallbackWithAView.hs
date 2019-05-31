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

module CallbackWithAView where

import qualified Data.Map as M
import Lorentz
import Michelson.Typed (BigMap)
import Michelson.Printer
import Tezos.Core (unsafeMkMutez)
import Prelude (Bool(..), Either, Int, Natural, Text, toText)
import GHC.Generics (Generic)

data Parameter
  = SetPause       Bool
  | GetBalance     (View Address Natural)
  deriving stock Generic
  deriving anyclass IsoValue

type LedgerValue = ("balance" :! Natural, "approvals" :! Map Address Natural)

data StorageFields = StorageFields
  { manager     :: Address
  , paused      :: Bool
  , totalSupply :: Natural
  } deriving stock Generic
    deriving anyclass IsoValue

data Storage = Storage
  { ledger :: BigMap Address LedgerValue
  , fields :: StorageFields
  } deriving stock Generic
    deriving anyclass IsoValue

contract2 :: Contract Parameter Storage
contract2 = do
  unpair
  caseT @Parameter
    ( #cSetPause /-> do
        dip (getField #fields); setField #paused; setField #fields
        nil; pair
    , #cGetBalance /-> view_ $ do
        unpair; dip ( toField #ledger ); get;
        ifSome (toField #balance) (push 0)
    )

printCallbackWithAViewContract :: Text
printCallbackWithAViewContract = toText $ printTypedContract $ compileLorentz contract2
