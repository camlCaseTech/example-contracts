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

tokenContract :: Contract Parameter Storage
tokenContract = do
  unpair
  caseT @Parameter
    ( #cInitToken /-> do
        get_ #owner; dip (swap); set_ #owner; swap;
        get_ #totalSupply; dip (swap); set_ #totalSupply; swap;
        get_ #decimals; dip (swap); set_ #decimals; swap;
        get_ #name; dip (swap); set_ #name;
        -- get_ #symbol; dip (swap); set_ #symbol;
        push (1 :: Natural); set_ #version;

        swap; createOwnerAccount;
        
        nil;
        pair;

    , #cApprove /-> do
        -- [ApproveParams, Storage]
        swap;       -- [Storage, ApproveParams]
        sender;     -- [Address, Storage, ApproveParams]
        getAccount; -- [Account, Storage, ApproveParams]

        dip swap # swap; -- [ApproveParams, Account, Storage]
        get_ #tokens;
        push (0 :: Natural);
        eq;

        -- [ApproveParams, Account, Storage]
        if_ ( do
                swap; unpair;             -- [Balance, Allowances, ApproveParams, Storage]
                dip ( dip (get_ #spender) -- [Balance, Allowances, spender, ApproveParams, Storage]
                    # swap                -- [Balance, spender, Allowances, ApproveParams, Storage]
                    # dip none # update); -- [Balance, Allowances, ApproveParams, Storage]              
                pair;
            )
            ( do
                get_ #spender;                -- [spender, ApproveParams, Account, Storage]
                dip (get_ #tokens # some)     -- [spender, some tokens, ApproveParams, Account, Storage]
                dip (dip (swap # dup # cdr)) -- [spender, some tokens, Allowances, Account, ApproveParams, Storage]
                update;                    -- [Allowances, Account, ApproveParams, Storage]
                -- failWith
                swap; car; pair
            );
        -- [Account, ApproveParams, Storage]
        some
        dip (swap # get_ #accounts) -- [Account, Accounts, Storage, ApproveParams]
        sender
        update

        set_ #accounts
        dip drop
        nil; pair;
    )

createOwnerAccount :: '[InitParams, Storage] :-> '[Storage]
createOwnerAccount = do
  dip (get_ #accounts); -- [InputParams, Accounts, Storage]

  get_ #totalSupply;
  emptyMap;
  swap;
  pair; 
  some; -- [Option (Balance, Allowances), InputParams, Accounts, Storage]
  
  swap;
  access_ #owner; -- [owner, Option (Balance, Allowances), Accounts, Storage]

  update;
  set_ #accounts;

getAccount :: Address & Storage & s :-> Account & Storage & s
getAccount = do
  dip (get_ #accounts); get; ifNone ( emptyMap # push 0 # pair ) nop

printLorentzExample :: Text
printLorentzExample = toText $ printTypedContract $ compileLorentz tokenContract
