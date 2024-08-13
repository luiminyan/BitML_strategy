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


-- compare labels, for transformRun (append in dictionary)
instance Ord (Label ConcID) where
  compare LSplit{} LAuthReveal{} = LT
  compare LAuthReveal{} LPutReveal{} = LT
  compare LPutReveal{} LWithdraw{} = LT
  compare LWithdraw{} LAuthControl{} = LT  


{- CV function in BitML paper. Extract the ID (CID | VID) in a label.
    If label = Split / Withdraw / Put, return [id]
    else return [] (in paper: an empty set)
-}
cv :: Label id -> [id]
cv l =
    case l of
        LSplit id _ -> [id]
        LPutReveal _ _ _ id _ -> [id]
        LWithdraw _ _ id -> [id]
        _ -> []



putOrSplit :: Label id -> Bool
putOrSplit (LPutReveal _ _ _ id _) = True
putOrSplit (LSplit id _) = True
putOrSplit _ = False

