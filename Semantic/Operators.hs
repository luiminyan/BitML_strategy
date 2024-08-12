module Semantic.Operators where
import Syntax.Label
import Syntax.Common (ID (..), Participant (Participant), Money (BCoins), Pred (..), Secret (Secret), Time (..), E (..), subTime, VarID (..), ConcID (..))
import NewSet
import Syntax.Contract 
import Syntax.Run
import Syntax.Strategy 
import Semantic.Environment
import qualified Data.Map as Map


-- greedyCombination of two Strategy Result Actions list
greedyActionsCombination :: Eq id => NewSet (Label id) -> NewSet (Label id) -> NewSet (Label id)
greedyActionsCombination s1 s2 =
        let s1_cv_result = toListSet cv s1 in                                                          -- store cv(label) in a list
        let s2_empty = filterSet (\label -> cv label == []) s2 in                                  -- cv(label') = empty
            let s2_not_empty = negationSet s2 s2_empty in                                               -- cv(label') = not empty
                let s2_unique = filterSet (\label -> cv label `notElem` s1_cv_result) s2_not_empty in   -- cv(label') not in cv(label) list
                    let selectedS2 = filterSet (elemSet (unionSet s2_empty s2_unique)) s2 in            -- apply original label order of s2
                        unionSet s1 selectedS2



-- get current time from run
getCurrentTime :: Run -> Time
getCurrentTime (Run (_, [])) = Time 0      -- init time = time 0
getCurrentTime (Run (_, [(_, _, t)])) = t
getCurrentTime (Run (init, x : xs)) = getCurrentTime (Run (init, xs))



-- minimum time from a contract
minTimeC :: Contract -> Time
minTimeC [] = TerminationTime           -- The minimum of nothing is infinite!
minTimeC gConList = minimum $ map minTimeG gConList



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
minTimeRun (Run (_, [])) = TerminationTime           -- The minimum of nothing is infinite!
minTimeRun (Run (_, [(_, configList, _)])) =                                               -- last tuple in the run
    let activeContractList = filter (\(ActiveContract contract _ _) -> True) configList in              -- configObj list with only active contracts
        let contractList = map (\(ActiveContract contract _ _) -> contract) activeContractList in       -- list of contracts from each active contract
        minimum $ map minTimeC contractList                                                             -- the minimum time in all contracts
minTimeRun (Run (init, x: xs)) = minTimeRun (Run (init, xs))



{- update environment with 'LPut' or 'LSplit' and all its successor(s)
    split or put [succ(s)], update
    other-labels [], orig. env
    (embedded function) updateHelper: append indexes

-}
updateAllSuccEnv :: Label ID -> [VarID] -> Environment -> Environment
updateAllSuccEnv label succList env = updateHelper label succList env 0
    where
        updateHelper :: Label ID -> [VarID] -> Environment  -> Int -> Environment
        updateHelper LSplit {} (x:xs) env index = updateHelper label xs (updateEnv x (label, index) env) (index + 1)
        updateHelper LPutReveal {} (x:xs) env index = updateHelper label xs (updateEnv x (label, index) env) (index + 1)
        updateHelper _ [] env _ = env



type SuccIdDict = Map.Map Int ConcID        -- {index: concrete successor id}



{- get the successor(new) active contract id from run, after a label is executed
    run wit
-}
getSuccDictFromRun :: Label ConcID -> Run -> SuccIdDict
getSuccDictFromRun label run =
    let transRun = transformRun run in
        case Map.lookup label transRun of
            Just ((confi, ti), (confj, tj)) ->
                let confiIdList = succIdList confi in
                    let confjIdList = succIdList confj in
                       let newIdList = filter (`notElem` confiIdList) confjIdList in
                            Map.fromList $ zip [0 ..] newIdList
            Nothing                         -> error "getSuccDictFromRun: predecessor Label does not exist in run!"
    where succIdList = foldl (\acc x -> case x of
                                            ActiveContract _ _ id -> acc ++ [id]
                                            Deposit _ _ id -> acc ++ [id]) []



resolveID :: ID -> Environment -> Run -> ConcID
resolveID (CID concId) _ _ = concId
resolveID (VID toResolveId) env run =
    case lookupEnv toResolveId env of
        Just (predLabel, index) ->
            let succIdDict = getSuccDictFromRun (resolveLabelID predLabel env run) run in       -- successor Id list of a label in run, possibly empty
                case Map.lookup index succIdDict of         -- lookup successor by index
                    Just resolvedId -> resolvedId
                    Nothing         -> error "resolveId: ID cannot be resolved!"
        Nothing -> error "resolveID: ID not found in environment!"



{- resolve the id in a label, helper function for eval do Label & ExecutedThenElse -}
resolveLabelID :: Label ID -> Environment -> Run -> Label ConcID
resolveLabelID label env run =
    case label of
        LSplit id gc                -> LSplit (resolveID id env run) gc
        LAuthReveal p s             -> LAuthReveal p s
        LPutReveal ds secs p id gc  -> LPutReveal ds secs p (resolveID id env run) gc
        LWithdraw p m id            -> LWithdraw p m (resolveID id env run)
        LAuthControl p id gc        -> LAuthControl p (resolveID id env run) gc



{-  check if a label is executed in a run
    init-run: false
-}
executedLabel :: Label ConcID -> Run -> Bool
executedLabel label run@(Run (_, tuples)) = any (\(l, _, _) -> l == label) tuples



-- eval Predicate
{- get the length of a secret recursivly from run, to be used in if-then-else pred
    if secret revealed: Just sLen
    else:               Nothing    
-}
getSecLen :: Secret -> Run -> Maybe Int
getSecLen _ (Run (_, [])) = Nothing
getSecLen s (Run (init, (_, confList, _) : xs)) =
    let revSecDict = Map.fromList [(sec, slen) | (RevealedSecret _ sec slen) <- confList] in
        case Map.lookup s revSecDict of
            Just slen   -> Just slen
            Nothing     -> getSecLen s (Run (init, xs))     -- recursivly check the next config tupel



{-  evaluate an artihmetic expression based on a run
    invalid expression: Nothing  
    valid:              Just <Int>
-}
evalArithExpr :: E -> Run -> Maybe Int
evalArithExpr e run =
    case e of
        EInt i      -> Just i
        ELength s   -> getSecLen s run
        EAdd e1 e2  ->
            case (evalArithExpr e1 run, evalArithExpr e2 run) of
                (Just i1, Just i2)  -> Just $ (+) i1 i2
                _                   -> Nothing
        ESub e1 e2  ->
            case (evalArithExpr e1 run, evalArithExpr e2 run) of
                (Just i1, Just i2)  -> Just $ (-) i1 i2
                _                   -> Nothing


{- evaluate a predicate based on a run -}
evalPred :: Pred -> Run -> Bool
evalPred PTrue _            = True
evalPred (PAnd p1 p2) run   = (&&) (evalPred p1 run) (evalPred p2 run)
evalPred (POr p1 p2) run    = (||) (evalPred p1 run) (evalPred p2 run)
evalPred (PNot p) run       = not (evalPred p run)
evalPred (PEq e1 e2) run    =
    case (evalArithExpr e1 run, evalArithExpr e2 run) of
        (Just n1, Just n2)  -> (==) n1 n2
        _                   -> error ""         -- TODO: failure!
evalPred (PNeq e1 e2) run   =
    case (evalArithExpr e1 run, evalArithExpr e2 run) of
        (Just n1, Just n2)  -> (/=) n1 n2
        _                   -> error ""         -- TODO: failure!
evalPred (PBtwn e1 e2 e3) run =
    case (evalArithExpr e1 run, evalArithExpr e2 run, evalArithExpr e3 run) of
        (Just n1, Just n2, Just n3) -> (&&) ((<=) n1 n2) ((<=) n2 n3)       -- n1 <= n2 <= n3
        _                           -> error ""
evalPred (PLt e1 e2) run =
    case (evalArithExpr e1 run, evalArithExpr e2 run) of
        (Just n1, Just n2)  -> (<=) n1 n2
        _                   -> error ""



-- evaluation : see AbstractExpr -> ConcreteExpr in FP course
eval :: Environment -> AbstractStrategy -> ConcreteStrategy
eval env (Do label) = \run ->
    Actions $ UnordSet [resolveLabelID label env run]       -- resolve id in the label

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
                (Actions s1, Actions s2)    ->
                    if isEmptySet s1                    -- possibly leads to Actions EmptySet => Delay min-time 
                        then Delay $ minTimeRun run
                        else Actions $ greedyActionsCombination s1 s2

eval env (ExecutedThenElse label succList as1 as2) = \run ->
    let env' = updateAllSuccEnv label succList env in           -- (temp.) env': update env with label: (succ, index)
        if executedLabel (resolveLabelID label env run) run     -- resolve id in a label 
            then eval env' as1 run                              -- label executed: update env
            else eval env as2 run

eval env (IfThenElse (CheckTimeOut t) as1 as2) =            -- if before t then as1 else as2
    \run -> let cs1 = eval env as1 run in
        let cs2 = eval env as2 run in
            let now = getCurrentTime run in
                if now < t then cs1 else cs2

eval env (IfThenElse (Predicate p) as1 as2) = \run ->       -- evaluate predicate
    if evalPred p run
        then eval env as1 run
        else eval env as2 run




-- main = do
--     -- test greedyActionsCombination
--     -- let s1 = fromListSet EmptySet [LWithdraw (Participant "A") (BCoins 1) (ConcID "x1"), LAuthControl (Participant "A") (ConcID "x1") (Withdraw (Participant "A"))]
--     -- let s4 = fromListSet EmptySet [LSplit (ConcID "x1") (Split []), LPutReveal [] [] PTrue (ConcID "x2") (Split []), LAuthControl (Participant "A") (ConcID "x1") (Withdraw (Participant "A")), LAuthReveal (Participant "A") (Secret "a")]
--     -- print $ (greedyActionsCombination s1 s4) == (UnordSet [LWithdraw (Participant "A") (BCoins 1) (ConcID "x1"),LAuthControl (Participant "A") (ConcID "x1") (Withdraw (Participant "A")),LPutReveal [] [] PTrue (ConcID "x2") (Split []),LAuthReveal (Participant "A") (Secret "a")])
--     -- True

--     -- TODO: test minTimeRun
--     -- print $ minTimeG (Split [])   not allowed by BitML semantics
--     -- let env0 = emptyEnv
--     -- let varList = [VarID "x1", VarID "x2", VarID "x3"]
--     -- let label2 = LWithdraw (Participant "A") (BCoins 3) (VID (VarID "x1"))
--     -- let label1 = LSplit (CID (ConcID "x")) (Split [(BCoins 1, []), (BCoins 1, []), (BCoins 1, [])])
--     -- let env1 = updateAllSuccEnv label1 varList env0
--     -- print env1
--     -- let env2 = updateAllSuccEnv label2 [VarID "y2"] env1
--     -- print env2
--     print "Operators.hs"

