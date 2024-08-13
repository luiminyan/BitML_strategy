module Syntax.Label (
    Label (..)
    , cv
    , putOrSplit
) where

import Syntax.Common ( Secret, Money, Participant, ConcID, Pred, Time )
import Syntax.Contract (GuardedContract)


-- type Label takes variable type as parameter
data Label id =
    LSplit id GuardedContract                               -- avoid ambigious names fors successors -> in the case of choice: split(x) + split(x)
    | LAuthReveal Participant Secret
    | LPutReveal [ConcID] [Secret] Pred id GuardedContract      -- right now: v. deposits have ConcID
    | LWithdraw Participant Money id
    | LAuthControl Participant id GuardedContract 
    | LDelay Time
    deriving (Eq, Show)




{- CV function in BitML paper. Extract the ID (CID | VID) in a label.
    If label = Split / Withdraw / Put, return [id]
    else return [] (in paper: an empty set)
-}
cv :: Label id -> [id]
cv l =
    case l of
        LSplit labelId _ -> [labelId]
        LPutReveal _ _ _ labelId _ -> [labelId]
        LWithdraw _ _ labelId -> [labelId]
        _ -> []



putOrSplit :: Label id -> Bool
putOrSplit LPutReveal {} = True
putOrSplit LSplit {} = True
putOrSplit _ = False

