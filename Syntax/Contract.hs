{-|
Module      : Syntax.Contract
Description : BitML contract
-}

module Syntax.Contract (
    Contract
    , GuardedContract (..)
) where

import Syntax.Common ( Secret, Money, Participant, ConcID, Time, Pred )


-- BitML contract: a collaction of all choices (GContacts)
type Contract = [GuardedContract] 


-- BitML guarded contract 
data GuardedContract =
    PutReveal [ConcID] [Secret] Pred Contract       -- v.deposits with ConcID
    | Withdraw Participant
    | Split [(Money, Contract)]
    | Auth [Participant] GuardedContract
    | After Time GuardedContract
    deriving (Eq, Show)
