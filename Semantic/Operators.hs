module Semantic.Operators where
import Syntax.Label ( Label(..) )
import Syntax.Common (ID (..), Participant (Participant), Money (BCoins), Pred (..), Secret (Secret), Time (..), E (..), subTime, VarID (..), ConcID (..))
import NewSet
import Syntax.Contract (GuardedContract(..), Contract)
import Syntax.Run (Run (Run), InitConfiguration (InitConfig), ConfigObject (..))
import Syntax.Strategy (AbstractStrategy (..), ConcreteStrategy, StrategyResult (..), Condition (..))
import Semantic.Environment

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



-- greedyCombination of two Strategy Result Actions list
greedyActionsCombination :: Eq id => NewSet (Label id) -> NewSet (Label id) -> NewSet (Label id)
greedyActionsCombination s1 s2 =
        let s1_cv_result = mapSetList cv s1 in                                                          -- store cv(label) in a list
        let s2_empty = filterSet (\label -> cv label == Nothing) s2 in                                  -- cv(label') = empty
            let s2_not_empty = negationSet s2 s2_empty in                                               -- cv(label') = not empty
                let s2_unique = filterSet (\label -> cv label `notElem` s1_cv_result) s2_not_empty in   -- cv(label') not in cv(label) list
                    let selectedS2 = filterSet (elemSet (unionSet s2_empty s2_unique)) s2 in            -- apply original label order of s2
                        unionSet s1 selectedS2



-- get current time from run
getCurrentTime :: Run -> Time
getCurrentTime (Run (InitConfig, [])) = Time 0      -- init time = time 0
getCurrentTime (Run (_, [(_, _, t)])) = t
getCurrentTime (Run (_, x : xs)) = getCurrentTime (Run (InitConfig, xs))



-- minimum time from a contract
minTimeC :: Contract -> Time
minTimeC [] = TerminationTime           -- The minimum of nothing is infinite!
minTimeC gContracts = minimum $ map minTimeG gContracts



-- minimum time from a guarded contract
minTimeG :: GuardedContract -> Time
minTimeG (PutReveal _ _ _ contract) = minTimeC contract         -- minimum time of contract
minTimeG (Withdraw _) = TerminationTime                         -- withdraw terminates the active contract
minTimeG (Split sList) = minimum $ map (minTimeC . snd) sList   -- minimum time of all splitted contratcs
minTimeG (Auth _ gc) = minTimeG gc
minTimeG (After t gc)
    | t >= minTimeG gc = minTimeG gc
    | otherwise = t


-- minimum time from active contracts in run
minTimeRun :: Run -> Time
minTimeRun (Run (InitConfig, [])) = TerminationTime           -- The minimum of nothing is infinite!
minTimeRun (Run (InitConfig, [(_, configList, _)])) =                                               -- last tuple in the run
    let activeContractList = filter (\(ActiveContract contract _ _) -> True) configList in              -- configObj list with only active contracts
        let contractList = map (\(ActiveContract contract _ _) -> contract) activeContractList in       -- list of contracts from each active contract
        minimum $ map minTimeC contractList                                                             -- the minimum time in all contracts
minTimeRun (Run (InitConfig, x: xs)) = minTimeRun (Run (InitConfig, xs))




{- get the length of a secret, to be used in if-then-else pred
    if secret revealed: Just sLen
    else:               Nothing    
-}

getSecLen :: Secret -> Run -> Maybe Int
getSecLen _ (Run (InitConfig, [])) = Nothing
getSecLen s (Run (InitConfig, [(_, confList, _)])) =
    let revSecList = foldl (\acc (RevealedSecrect _ secret sLen) -> (secret, sLen) : acc) [] confList in
        case revSecList of
            [] -> Nothing
            (sec, len) : xs -> if sec == s then Just len else Nothing



{- updateAllSuccEnv, update environment with all successors
-}
updateAllSuccEnv :: Label ID -> [VarID] -> Environment -> Environment
updateAllSuccEnv label idList env = updateHelper label idList env 0
    where 
        updateHelper :: Label ID -> [VarID] -> Environment  -> Int -> Environment
        updateHelper _ [] env _ = env
        updateHelper label (x:xs) env index = updateHelper label xs (updateEnv x (label, index) env) (index + 1)



{- TODO: resolve VarID into ConcID
    search for the predecessor of VarID in Environment till a ConcID
    replace VarID with the corresponding ConcID in run
-}
resolveID :: ID -> Environment -> Run -> ConcID
resolveID (CID concId) _ _ = concId
resolveID (VID (VarID id)) _ _ = ConcID id


{-  check if a label is executed in a run
    init-run: false
-}
executedLabel :: Label ConcID -> Run -> Bool
executedLabel label run@(Run (_, tuples)) = any (\(l, _, _) -> l == label) tuples



{- TODO for eval
    1. update 'do label' with 'resolve id'

    *. finish IfElseThen predicate
-}


-- evaluation : see AbstractExpr -> ConcreteExpr in FP course
eval :: Environment -> AbstractStrategy -> ConcreteStrategy
eval env (Do label) = \run ->
    case label of
        LSplit id gc                -> Actions $ UnordSet [LSplit (resolveID id env run) gc]
        LAuthReveal p s             -> Actions $ UnordSet [LAuthReveal p s]
        LPutReveal ds secs p id gc  -> Actions $ UnordSet [LPutReveal ds secs p (resolveID id env run) gc]
        LWithdraw p m id            -> Actions $ UnordSet [LWithdraw p m (resolveID id env run)]
        LAuthControl p id gc        -> Actions $ UnordSet [LAuthControl p (resolveID id env run) gc]


eval env DoNothing = Delay . minTimeRun         -- DoNothing = delay minimum time from active contract(s) in run | termination-time

eval env (WaitUntil (Time d)) =
    \run -> let now = getCurrentTime run in
        if now < Time d then Delay $ subTime (Time d) now
        else error "Invalid strategy: time to wait already past"

eval env (Combination as1 as2) = \run ->
    let cs1 = eval env as1 in               -- as1 and as2 are parallel evaluated
        let cs2 = eval env as2 in
            case (cs1 run, cs2 run) of
                (Delay t1, Delay t2)        -> Delay $ min t1 t2
                (Delay t1, as2)             -> as2
                (as1, Delay t2)             -> as1
                (Actions s1, Actions s2)    -> Actions $ greedyActionsCombination s1 s2

eval env (ExecutedThenElse label succList as1 as2) = \run ->
    case label of
        LAuthReveal p s -> 
            if executedLabel (LAuthReveal p s) run 
                then eval env as1 run 
                else eval env as2 run
        LPutReveal depoList secList p id gc -> 
            let env' = updateEnv (head succList) (label, 0) env in
                if executedLabel (LPutReveal depoList secList p (resolveID id env run) gc) run      -- if label elem* run
                    then eval env' as1 run      -- eval as1 with the updated env
                    else eval env as2 run
        LWithdraw p m id -> 
            if executedLabel (LWithdraw p m (resolveID id env run)) run 
                then eval env as1 run 
                else eval env as2 run
        LAuthControl p id gc -> 
            if executedLabel (LAuthControl p (resolveID id env run) gc) run 
                then eval env as1 run 
                else eval env as2 run        




eval env (IfThenElse (CheckTimeOut t) as1 as2) =
    \run -> let cs1 = eval env as1 run in
        let cs2 = eval env as2 run in
            let now = getCurrentTime run in
                if t < now then cs1 else cs2

-- TODO: finish if-else-then predicate
eval env (IfThenElse (Predicate PTrue) as1 _) = eval env as1







main = do
    -- -- cv tested!
    -- let l1 = LWithdraw (Participant "A") (BCoins 1) (VarID "x1")
    -- let l2 = LAuthControl (Participant "A") (ConcID "x1") (Withdraw (Participant "A"))
    -- let l3 = LSplit (CID (ConcID "x1")) (Split [])
    -- print $ cv l1       -- Just [VarID "x1"]
    -- print $ cv l2       -- Nothing
    -- print $ cv l3       -- Just [CID (ConcID "x1")]

    -- test greedyActionsCombination
    -- let s1 = insertList EmptySet [LWithdraw (Participant "A") (BCoins 1) (ConcID "x1"), LAuthControl (Participant "A") (ConcID "x1") (Withdraw (Participant "A"))]
    -- let s4 = insertList EmptySet [LSplit (ConcID "x1") (Split []), LPutReveal [] [] PTrue (ConcID "x2") (Split []), LAuthControl (Participant "A") (ConcID "x1") (Withdraw (Participant "A")), LAuthReveal (Participant "A") (Secret "a")]

    -- print $ greedyActionsCombination s1 s4      -- UnordSet [LWithdraw (Participant "A") (BCoins 1) (ConcID "x1"),LAuthControl (Participant "A") (ConcID "x1") (Withdraw (Participant "A")),LPutReveal [] [] PTrue (ConcID "x2") (Split []),LAuthReveal (Participant "A") (Secret "a")]


    -- TODO: test minTimeRun
    -- print $ minTimeG (Split [])   not allowed by BitML semantics
    -- let env0 = emptyEnv
    -- let varList = [VarID "x1", VarID "x2", VarID "x3"]
    -- let label2 = LWithdraw (Participant "A") (BCoins 3) (VID (VarID "x1"))
    -- let label1 = LSplit (CID (ConcID "x")) (Split [(BCoins 1, []), (BCoins 1, []), (BCoins 1, [])])
    -- let env1 = updateAllSuccEnv label1 varList env0
    -- print env1
    -- let env2 = updateAllSuccEnv label2 [VarID "y2"] env1
    -- print env2
    print "Operators.hs"

