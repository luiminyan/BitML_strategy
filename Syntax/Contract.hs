{-|
Module      : Syntax.Contract
Description : BitML contract
-}

module Syntax.Contract where

import Syntax.Common ( Secret, Money, Participant, ID, Time, Pred )

-- BitML contract: a collaction of all choices (GContacts)
type Contract = [GuardedContract] 


-- BitML guarded contract 
data GuardedContract =
    PutReveal [ID] [Secret] Pred Contract
    | Withdraw Participant
    | Split [(Money, Contract)]
    | Auth [Participant] GuardedContract
    | After Time GuardedContract
    deriving (Eq, Show)
