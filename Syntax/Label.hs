module Syntax.Label (
    Label (..)
    , cv
    , putOrSplit
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



-- main = do
    -- -- cv tests passed!
    -- let l1 = LWithdraw (Participant "A") (BCoins 1) (VarID "x1")
    -- let l2 = LAuthControl (Participant "A") (ConcID "x1") (Withdraw (Participant "A"))
    -- let l3 = LSplit (CID (ConcID "x1")) (Split [])
    -- print $ cv l1       -- Just [VarID "x1"]
    -- print $ cv l2       -- Nothing
    -- print $ cv l3       -- Just [CID (ConcID "x1")]