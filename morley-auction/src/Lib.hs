{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lib
  ( auction
  , printAuction
  , printAuctionSplitLogic
  ) where

import Lorentz
import Michelson.Printer
import Prelude (Bool(..), Text, toText)

type Parameter = KeyHash

type AuctionStart    = Timestamp
type AuctionEnd      = Timestamp
type Beneficiary     = KeyHash
type ItemDescription = Text
type HighestBidder   = KeyHash
type HighestBid      = Mutez
type AuctionEnded    = Bool

type Storage =
  ( -- mutable storage
    ((AuctionStart, AuctionEnd), (Beneficiary, ItemDescription))
    -- immutable storage
  , ((HighestBidder, HighestBid), AuctionEnded)
  )

type Input = (Parameter, Storage)
type Output storage = ([Operation], storage)

printAuction :: Text
printAuction = toText $ printTypedContract $ compileLorentz auction

printAuctionSplitLogic :: Text
printAuctionSplitLogic = toText $ printTypedContract $ compileLorentz auctionSplitLogic

auction :: Contract Parameter Storage
auction = checkIfAuctionStarted

checkIfAuctionStarted :: '[ Input ] :-> '[ Output Storage ]
checkIfAuctionStarted =
  do dup; cdaaar; now; lt;
     if_
       ( push ("The auction has not started yet." :: Text) # failWith )
       ( now
       # dip (dup # cdaadr)
       # lt
       # if_
           ( dup # cddadr # amount # gt #
               if_
               ( dup # cddaar # implicitAccount # dip (dup # cddadr) # swap # unit # transferTokens # drop
               # dup # car # amount # swap # pair # dip (cdr # dup # cddr) # pair # dip (car) # swap # pair
               # nil # pair
               )
               ( push ("Your bid is lower than the highest bid. The highest current bid is:" :: Text)
               # dip (cdadr) # pair # failWith
               )
           )
           ( dup # cdddr  #
               if_ ( push ("The auction has already ended." :: Text) # failWith )
               ( dup # cdadar # implicitAccount # dip (dup # cddadr) # swap # unit # transferTokens # drop
                 # cdr # dup # cdar # push True # swap # pair # dip (car) # swap # pair #
                 nil # pair
               
               )
           )
       )


auctionSplitLogic :: Contract Parameter Storage
auctionSplitLogic = checkIfStarted

checkIfStarted :: '[ Input ] :-> '[ Output Storage ]
checkIfStarted = do dup; cdaaar; now; lt; if_ ( push ("The auction has not started yet." :: Text) # failWith ) ended

checkIfEnded :: '[ Input ] :-> '[ Output Storage ]
checkIfEnded = do now; dip (dup # cdaadr); lt; if_ ended notEnded

ended :: '[ Input ] :-> '[ Output Storage ]
ended = dup # cddadr # amount # gt # if_ setHighestBid rejectLowBid

setHighestBid :: '[ Input ] :-> '[ Output Storage ]
setHighestBid =
  dup # cddaar # implicitAccount # dip (dup # cddadr) # swap # unit # transferTokens # drop
  # dup # car # amount # swap # pair # dip (cdr # dup # cddr) # pair # dip (car) # swap # pair
  # nil # pair

rejectLowBid :: '[ Input ] :-> '[ Output Storage ]
rejectLowBid =
  push ("Your bid is lower than the highest bid. The highest current bid is:" :: Text)
  # dip (cdadr) # pair # failWith

notEnded :: '[ Input ] :-> '[ Output Storage ]
notEnded =
  dup # cdddr # if_ ( push ("The auction has already ended." :: Text) # failWith ) transferTokensToBeneficiary

transferTokensToBeneficiary ::  '[ Input ] :-> '[ Output Storage ]
transferTokensToBeneficiary =
  dup # cdadar # implicitAccount # dip (dup # cddadr) # swap # unit # transferTokens # drop
      # cdr # dup # cdar # push True # swap # pair # dip (car) # swap # pair #
        nil # pair         
     
           


-- callingConvention :: '[ Input ] :-> '[ Output Storage ]
-- callingConvention = do cdr; nil; pair

-- macros

cdadr :: (a1, ((a2, a3), a4)) & s :-> a3 & s
cdadr = cdr # car # cdr

cdddr :: (a1, (a2, (a3, a4))) & s :-> a4 & s
cdddr = cdr # cdr # cdr

cdaaar :: (a1, (((a2, a3), a4), b)) & s :-> a2 & s
cdaaar = cdr # car # car # car

cdaadr :: (a1, (((a2, a3), a4), b)) & s :-> a3 & s
cdaadr = cdr # car # car # cdr

cdadar :: (a1, ((a2, (a3, a4)), b)) & s :-> a3 & s
cdadar = cdr # car # cdr # car

cddaar :: (a1, (a2, ((a3, a4), a5))) & s :-> a3 & s
cddaar = cdr # cdr # car # car

cddadr :: (a1, (a2, ((a3, a4), a5))) & s :-> a4 & s
cddadr = cdr # cdr # car # cdr

{-
cdaaar

unpair :: (a, b) & s :-> a & b & s
unpair = dup # car # dip cdr

cdar :: (a1, (a2, b)) & s :-> a2 & s
cdar = cdr # car

cddr :: (a1, (a2, b)) & s :-> b & s
cddr = cdr # cdr

caar :: ((a, b1), b2) & s :-> a & s
caar = car # car

cadr :: ((a, b1), b2) & s :-> b1 & s
cadr = car # cdr



type Parameter = KeyHash

type AuctionEnd = Timestamp
type Bid = (Mutez, KeyHash)

type Storage = (AuctionEnd, Bid)

type Input = (Parameter, Storage)
type Output storage = ([Operation], storage)

auction :: Contract Parameter Storage
auction = do
  checkIfAuctionHasEnded
  setupReplacementStorage
  checkNewBidIsGreater
  getRefund
  makeRefund
  callingConvention

checkIfAuctionHasEnded :: '[ Input ] :-> '[ Input, Timestamp ]
checkIfAuctionHasEnded = do dup; cdar; dup; now; gt; if_ fail_ nop; swap

setupReplacementStorage :: '[ Input, Timestamp] :-> '[ Bid, Storage ]
setupReplacementStorage =
  do dup; car; dip cddr; amount; pair; swap; dip (swap # pair)

checkNewBidIsGreater :: '[ Bid, Storage ] :-> '[ Bid, Storage ]
checkNewBidIsGreater = do dup; car; amount; le; if_ fail_ nop

getRefund :: '[ Bid, Storage ] :-> '[ Mutez, Bid, Storage ]
getRefund = do dup; car

makeRefund :: '[ Mutez, Bid, Storage ] :-> '[ Operation, Storage ]
makeRefund = do dip (cdr # implicitAccount); unit; transferTokens

callingConvention :: '[ Operation, Storage ] :-> '[ Output Storage ]
callingConvention = do nil; swap; cons; pair
-}
