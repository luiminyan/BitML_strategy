module Syntax.Label (
    Label (..)
    , cv
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



{- CV function in BitML paper. Extract the ID (CID | VID) in a label.
    If label = Split / Withdraw / Put, return [id]
    else return Nothing (in paper: an empty set)
-}
cv :: Label id -> Maybe [id]
cv l =
    case l of
        LSplit id _ -> Just [id]
        LPutReveal _ _ _ id _ -> Just [id]
        LWithdraw _ _ id -> Just [id]
        _ -> Nothing