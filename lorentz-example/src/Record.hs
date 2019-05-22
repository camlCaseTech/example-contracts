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
        get_ #system;
        dip swap;
        set_    #system;
        -- [Storage, Input]
        
        -- get the owner amount
        swap;
        get_ #totalSupply;
        push ("owner" :: Text);
        pair;
        -- [Account, Input, Storage]
        
        -- sender is the owner
        dip (swap # get_ #accounts); -- [Account, Accounts, Storage, Input]
        some;
        sender;
        update
        
        -- dip drop;
        -- nil; pair
        failWith
    )
