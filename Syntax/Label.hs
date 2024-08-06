module Syntax.Label (
    Label (..)
) where

import Syntax.Common ( Secret, Money, Participant, ConcID, Pred )
import Syntax.Contract (GuardedContract)


-- type Label takes variable type as parameter
data Label id =
    LSplit id GuardedContract                               -- avoid ambigious names fors successors -> in the case of choice: split(x) + split(x)
    | LAuthReveal Participant Secret
    | LPutReveal [ConcID] [Secret] Pred id GuardedContract      -- right now: v. deposits have ConcID
    | LWithdraw Participant Money id
    | LAuthControl Participant id GuardedContract 
    deriving (Eq, Show)
