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

module Lib where

import qualified Data.Map as M
import Lorentz
import Michelson.Typed (BigMap)
import Michelson.Printer
import Prelude (Bool(..), Int, Natural, Text, toText)
import GHC.Generics (Generic)

type Balance     = Natural
type Allowances  = Map Address Natural
type Account     = (Balance, Allowances)

-- parameter
data Parameter
  = InitToken InitParams
  | Approve   ApproveParams
  deriving stock Generic
  deriving anyclass IsoValue

type InitParams = ( "owner"       :! Address
                  , "totalSupply" :! Natural
                  , "decimals"    :! Natural
                  , "name"        :! Text
                  )

type ApproveParams = ( "spender" :! Address
                     , "tokens"  :! Natural
                     )

data Storage =
  Storage
--    { accounts    :: BigMap Address Account
    { accounts    :: M.Map Address Account
    , version     :: Natural
    , totalSupply :: Natural
    , decimals    :: Natural
    , name        :: Text
    , symbol      :: Text
    , owner       :: Address
    }
    deriving stock Generic
    deriving anyclass IsoValue

exampleContract :: Contract Parameter Storage
exampleContract = do
  unpair
  caseT @Parameter
    ( #cInitToken /-> do
        getField #owner; dip (swap); setField #owner; swap;
        getField #totalSupply; dip (swap); setField #totalSupply; swap;
        getField #decimals; dip (swap); setField #decimals; swap;
        getField #name; dip (swap); setField #name;
        -- getField #symbol; dip (swap); setField #symbol;
        push (1 :: Natural); setField #version;

        swap; createOwnerAccount;
        
        nil;
        pair;

    , #cApprove /-> do
        -- [ApproveParams, Storage]
        swap;       -- [Storage, ApproveParams]
        sender;     -- [Address, Storage, ApproveParams]
        getAccount; -- [Account, Storage, ApproveParams]

        dip swap # swap; -- [ApproveParams, Account, Storage]
        getField #tokens;
        push (0 :: Natural);
        eq;

        -- [ApproveParams, Account, Storage]
        if_ ( do
                swap; unpair;             -- [Balance, Allowances, ApproveParams, Storage]
                dip ( dip (getField #spender) -- [Balance, Allowances, spender, ApproveParams, Storage]
                    # swap                -- [Balance, spender, Allowances, ApproveParams, Storage]
                    # dip none # update); -- [Balance, Allowances, ApproveParams, Storage]              
                pair;
            )
            ( do
                getField #spender;                -- [spender, ApproveParams, Account, Storage]
                dip (getField #tokens # some)     -- [spender, some tokens, ApproveParams, Account, Storage]
                dip (dip (swap # dup # cdr)) -- [spender, some tokens, Allowances, Account, ApproveParams, Storage]
                update;                    -- [Allowances, Account, ApproveParams, Storage]
                -- failWith
                swap; car; pair
            );
        -- [Account, ApproveParams, Storage]
        some
        dip (swap # getField #accounts) -- [Account, Accounts, Storage, ApproveParams]
        sender
        update

        setField #accounts
        dip drop
        nil; pair;
    )

createOwnerAccount :: '[InitParams, Storage] :-> '[Storage]
createOwnerAccount = do
  dip (getField #accounts); -- [InputParams, Accounts, Storage]

  getField #totalSupply;
  emptyMap;
  swap;
  pair; 
  some; -- [Option (Balance, Allowances), InputParams, Accounts, Storage]
  
  swap;
  toField #owner; -- [owner, Option (Balance, Allowances), Accounts, Storage]

  update;
  setField #accounts;

getAccount :: Address & Storage & s :-> Account & Storage & s
getAccount = do
  dip (getField #accounts); get; ifNone ( emptyMap # push 0 # pair ) nop

printLorentzExample :: Text
printLorentzExample = toText $ printTypedContract $ compileLorentz exampleContract
