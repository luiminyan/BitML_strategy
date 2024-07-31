module Semantic.Operators where

import Syntax.Label
import Syntax.Common (ID (ID), Participant (Participant), Money (BCoins), Pred (..), Secret (Secret), Time (..), subTime, addTime)
import NewSet ( NewSet (..), filterSet, unionSet, mapSetList, negationSet, insertList )
import Syntax.Contract (GuardedContract(..), Contract)
import Syntax.Run (Run (Run), InitConfiguration (InitConfig), ConfigObject (..))
import Syntax.Strategy (AbstractStrategy (..), ConcreteStrategy, StrategyResult (..))
import Data.Maybe (mapMaybe)       -- map, but only keep values of 'Just' in a list

{- CV function in BitML paper. Extract the ID in a label.
    If label = Split / Withdraw / Put, return [id]
    else return Nothing (in paper: an empty set)
-}
cv :: Label -> Maybe [ID]
cv l =
    case l of
        LSplit id -> Just [id]
        LPutReveal _ _ _ id -> Just [id]
        LWithDraw _ _ id -> Just [id]
        _ -> Nothing



-- greedyCombination of two Strategy Result Actions list
greedyActionsCombination :: NewSet Label -> NewSet Label -> NewSet Label
greedyActionsCombination s1 s2 =
        let s1_cv_result = mapSetList cv s1 in                                                          -- store cv(label) in a list
        let s2_empty = filterSet (\label -> (cv label) == Nothing) s2 in                                -- cv(label') = empty
            let s2_not_empty = negationSet s2 s2_empty in                                               -- cv(label') = not empty
                let s2_unique = filterSet (\label -> notElem (cv label) s1_cv_result) s2_not_empty in   -- cv(label') not in cv(label) list
        unionSet s1 (unionSet s2_empty s2_unique)



-- -- get current time from run
-- getCurrentTime :: Run -> Time
-- getCurrentTime (Run (InitConfig, [])) = Time 0      -- init time = time 0
-- getCurrentTime (Run (_, [(_, _, t)])) = t
-- getCurrentTime (Run (_, x : xs)) = getCurrentTime (Run (InitConfig, xs))



-- -- minimum time from a contract
-- minTimeC :: Contract -> Time
-- minTimeC [] = error "minTimeC: empty contract!"
-- minTimeC gContracts = minimum $ map minTimeG gContracts



-- -- minimum time from a guarded contract
-- minTimeG :: GuardedContract -> Time
-- minTimeG (PutReveal _ _ _ contract) = minTimeC contract         -- minimum time of contract
-- minTimeG (Withdraw _) = TerminationTime                         -- withdraw terminates the active contract
-- minTimeG (Split []) = error "minTimeG: invalid contract, empty split"
-- minTimeG (Split sList) = minimum $ map (minTimeC . snd) sList   -- minimum time of all splitted contratcs
-- minTimeG (Auth _ gc) = minTimeG gc
-- minTimeG (After t gc)
--     | t >= minTimeG gc = minTimeG gc
--     | otherwise = t


-- -- minimum time in a run
-- minTimeRun :: Run -> Time
-- minTimeRun (Run (InitConfig, [(_, configList, _)])) =                                                     -- last tuple in the run
--     let activeContractList = filter (\(ActiveContract contract _ _) -> True) configList in          -- configObj list with only active contracts
--         let contractList = map (\(ActiveContract contract _ _) -> contract) activeContractList in   -- list of contracts from each active contract
--         minimum $ map minTimeC contractList                                                         -- the minimum time in all contracts
-- minTimeRun (Run (InitConfig, x: xs)) = minTimeRun (Run (InitConfig, xs))



-- evaluation : see AbstractExpr -> ConcreteExpr in FP course
eval :: AbstractStrategy -> ConcreteStrategy
eval (Do label) = \_ -> Actions $ UnordSet [label]
-- eval DoNothing = Delay . minTimeRun
-- eval (WaitUntil (Time d)) =
--     \run -> let now = getCurrentTime run in
--         if now < Time d then Delay $ subTime (Time d) now
--         else error "Invalid strategy: time to wait already past"
eval (Combination as1 as2) = \run ->
    let sr1 = eval as1 run in               -- cs1 run = sr1
        let sr2 = eval as2 run in
            case (sr1, sr2) of
                (Delay t1, Delay t2)        -> Delay $ min t1 t2
                (Delay t1, as2)             -> as2
                (as1, Delay t2)             -> as1
                (Actions s1, Actions s2)    -> Actions $ greedyActionsCombination s1 s2



main = do
    let s1 = insertList EmptySet [LWithDraw (Participant "A") (BCoins 1) (ID "x1"), LAuthControl (Participant "A") (ID "x1") (Withdraw (Participant "A"))]
    let s4 = insertList EmptySet [LSplit (ID "x1"), LPutReveal [] [] PTrue (ID "x1"), LAuthControl (Participant "A") (ID "x1") (Withdraw (Participant "A")), LAuthReveal (Participant "A") (Secret "a")]
    print "Operations.hs"
    print $ greedyActionsCombination s1 s4