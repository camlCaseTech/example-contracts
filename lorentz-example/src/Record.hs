{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NoApplicativeDo        #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE OverloadedLabels, ViewPatterns #-}

-- test adding embeded records to Storage
-- and see if we can use the getters and accessors on it

module Record where

import qualified Data.Map as M
import Lorentz
import Michelson.Typed (BigMap)
import Michelson.Printer
import Prelude (Bool(..), Int, Natural, Text, toText)
import GHC.Generics (Generic)

data Parameter
  = Init InitParams
  deriving stock Generic
  deriving anyclass IsoValue

type InitParams = ( "system"      :! Text
                  , "totalSupply" :! Natural
                  )

data W = W { totalSupply :: Natural }
  deriving stock Generic
  deriving anyclass IsoValue

data X =
  X
    { destination :: Address
    , amount :: Natural
    }
  deriving stock Generic
  deriving anyclass IsoValue


-- type Account = ( "name"    :! Text
--                , "balance" :! Natural
--                )

-- data Account =
--   Account
--     { name    :: Text
--     , balance :: Natural
--     }
--     deriving stock Generic
--     deriving anyclass IsoValue

type Account = ( Text
               , Natural
               )

type Accounts = BigMap Address Account

data Storage =
  Storage
    { accounts :: Accounts
    , system   :: Text
    }
    deriving stock Generic
    deriving anyclass IsoValue

recordContract :: Contract Parameter Storage
recordContract = do
  unpair
  caseT @Parameter
    ( #cInit /-> do
        -- get the name of the system and set it
        -- getField #system;
        -- ddd;
        -- push (1 :: Natural);

        -- construct $
        --   fieldCtor (push (2 :: Natural))
        --   :& RNil :: W & s
        push (2 :: Natural)
        toW

        eee;
        drop
        drop
--        drop
--        drop
        
        getField #system;
        dip swap;
        setField    #system;
        -- [Storage, Input]
        
        -- get the owner amount
        swap;
        getField #totalSupply;
        -- fieldCtor "balance";
        push ("owner" :: Text);
        -- fieldCtor "name";
        -- construct;
        -- wrap_
        -- [Account, Input, Storage]
        
        -- sender is the owner
        -- dip (swap # getField #accounts); -- [Account, Accounts, Storage, Input]
        -- some;
        -- sender;
        -- update
        
        -- dip drop;
        -- nil; pair
        failWith
    )

ddd :: (HasFieldOfType a "system" Text) => a & s :-> s
ddd = drop


toW :: Natural & s :-> W & s
toW = do
  construct $
    fieldCtor dup
    :& RNil
  dip drop

toX :: s :-> X & s
toX =
  construct $
       fieldCtor (sender)
    :& fieldCtor (push (0 :: Natural))
    :& RNil

toX2 :: Address & s :-> X & s
toX2 = do
  construct $
       fieldCtor (dup)
    :& fieldCtor (push (0 :: Natural))
    :& RNil
  dip drop

toX3 :: Address & Natural & s :-> X & s
toX3 = do
  construct $
       fieldCtor (dup)
    :& fieldCtor (push (0 :: Natural))
    :& RNil
  dip (drop # drop)

toX4 :: Address & Natural & s :-> X & s
toX4 = do
  construct $
       fieldCtor (dup)
    :& fieldCtor (push (0 :: Natural))
    :& RNil
  dip (drop)
  swap; setField #amount


eee :: (HasFieldOfType a "totalSupply" Natural) => a & s :-> Natural & a & s
-- eee :: W & s :-> Natural & W & s
eee = getField #totalSupply # push (1 :: Natural) # add
