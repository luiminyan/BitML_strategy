module Semantic.Operators (
    greedyActionsCombination,
    getCurrentTime,
    minTimeC,
    minTimeG,
    minTimeRun,
    updateAllSuccEnv,
    getSuccListFromRun,
    resolveID,
    resolveLabelID,
    executedLabel,
    getSecLen,
    evalArithExpr,
    evalPred,
    eval
) where
import Syntax.Label
import Syntax.Common
import NewSet
import Syntax.Contract 
import Syntax.Run
import Syntax.Strategy 
import Semantic.Environment
import qualified Data.Map as Map


-- if a label is excutable: ID exist in the last configuration 
executable :: Label ConcID -> Run -> Bool
executable label run =
    case label of
        LSplit lid _             -> lid `elem` activeContractsIDs                             
        LPutReveal _ _ _ lid _   -> lid `elem` activeContractsIDs
        LWithdraw _ _ lid        -> lid `elem` activeContractsIDs
        LAuthControl _ lid _     -> lid `elem` activeContractsIDs
        _ -> True               
    where activeContractsIDs = [labelId | (ActiveContract _ _ labelId) <- lastConfig run]



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
getCurrentTime (Run (initialConfig, _ : xs)) = getCurrentTime (Run (initialConfig, xs))



-- minimum time from a contract
minTimeC :: Contract -> Time
minTimeC [] = TerminationTime           -- The minimum of nothing is infinite!
minTimeC contract = minimum $ map minTimeG contract


{- minimum time of a guarded contract
    Withdraw: termintates the current active contract, returns termination time
    After t: minimum between t and min-time from the followed up guarded contract
    other labels: min-time of the followed up contract
-}
minTimeG :: GuardedContract -> Time
minTimeG (PutReveal _ _ _ contract) = minTimeC contract         -- minimum time of contract
minTimeG (Withdraw _) = TerminationTime                         -- withdraw terminates the active contract
minTimeG (Split sList) = minimum $ map (minTimeC . snd) sList   -- minimum time of all splitted contratcs
minTimeG (Auth _ gc) = minTimeG gc
minTimeG (After t gc)
    | t >= minTimeG gc = minTimeG gc
    | otherwise = t


{- minimum time from Configuration 
    without active contract: TerminationTime (The minimum of nothing is infinite!)
    else: minimum of minTime from all contracts
-}
minTimeConfiguration :: Configuration -> Time
minTimeConfiguration confList = 
    case contractList of
        []  -> TerminationTime     -- The minimum of nothing is infinite!
        _   -> minimum $ map minTimeC contractList
    where contractList = [c | (ActiveContract c _ _) <- confList]


{- minimum time in a run
    only initialized run: minTime from initConfig
    else: minTime from the last Configuration (in the last tuple)
-}
minTimeRun :: Run -> Time
minTimeRun run =
    case run of
        Run (initConf, []) -> minTimeConfiguration initConf
        Run (_, [(_, configList, _)]) -> minTimeConfiguration configList   
        Run (initialConf, _ : xs) -> minTimeRun (Run (initialConf, xs))
   




{- update environment with 'LPut' or 'LSplit' and all its successor(s)
    split or put [succ(s)], update
    other-labels [], orig. env
    (embedded function) updateHelper: append indexes

-}
updateAllSuccEnv :: Label ID -> [VarID] -> Environment -> Environment
updateAllSuccEnv label succList env = updateHelper label succList env 0
    where
        updateHelper :: Label ID -> [VarID] -> Environment  -> Int -> Environment
        updateHelper LSplit {} (x:xs) environment index = updateHelper label xs (updateEnv x (label, index) environment) (index + 1)
        updateHelper LPutReveal {} (x:xs) environment index = updateHelper label xs (updateEnv x (label, index) environment) (index + 1)
        updateHelper _ _ environment _ = environment




{- get the successor(new) active contract id from run, after a label is executed
    run wit
-}
getSuccListFromRun :: Label ConcID -> Run -> [ConcID]
getSuccListFromRun label run =
    let transRun = transformRun run in
        case searchTransRun label transRun of
            Just ((confi, _), (confj, _)) ->
                let confiIdList = succIdList confi in
                    let confjIdList = succIdList confj in
                       filter (`notElem` confiIdList) confjIdList                             
            Nothing                         -> error $ "getSuccListFromRun: predecessor label does not exist in run!" ++ show transRun
    where succIdList = foldl (\acc x -> case x of
                                            ActiveContract _ _ contractId -> acc ++ [contractId]
                                            Deposit _ _ depositID -> acc ++ [depositID]
                                            _ -> acc) []



resolveID :: ID -> Environment -> Run -> ConcID
resolveID (CID concId) _ _ = concId
resolveID (VID toResolveId) env run =
    case lookupEnv toResolveId env of
        Just (predLabel, index) ->
            let succIdList = getSuccListFromRun (resolveLabelID predLabel env run) run in       -- successor Id list of a label in run, possibly empty   
                if index <= length succIdList - 1 
                    then succIdList !! index
                    else error "resolveId: index out of range!"
        Nothing -> error $ "resolveID: ID not found in environment!" ++ show toResolveId



{- resolve the id in a label, helper function for eval do Label & ExecutedThenElse -}
resolveLabelID :: Label ID -> Environment -> Run -> Label ConcID
resolveLabelID label env run =
    case label of
        LSplit labelId c                -> LSplit (resolveID labelId env run) c
        LAuthReveal p s                 -> LAuthReveal p s
        LPutReveal ds secs p labelId gc -> LPutReveal ds secs p (resolveID labelId env run) gc
        LWithdraw p m labelId           -> LWithdraw p m (resolveID labelId env run)
        LAuthControl p labelId gc       -> LAuthControl p (resolveID labelId env run) gc
        LDelay t                        -> LDelay t



{-  check if a label is executed in a run
    LDelay: error, not allowed to use here
    init-run: false
-}
executedLabel :: Label ConcID -> Run -> Bool
executedLabel label (Run (_, tuples)) = any (\(l, _, _) -> l == label) tuples



-- eval Predicate
{- get the length of a secret recursivly from run, to be used in if-then-else pred
    if secret revealed: Just sLen
    else:               Nothing    
-}
getSecLen :: Secret -> Run -> Maybe Int
getSecLen _ (Run (_, [])) = Nothing
getSecLen s (Run (initialConfig, (_, confList, _) : xs)) =
    let revSecDict = Map.fromList [(sec, slen) | (RevealedSecret _ sec slen) <- confList] in
        case Map.lookup s revSecDict of
            Just slen   -> Just slen
            Nothing     -> getSecLen s (Run (initialConfig, xs))     -- recursivly check the next config tupel



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
        _                   -> error "evalPred: predicate not evaluatable!"         -- TODO: failure!
evalPred (PNeq e1 e2) run   =
    case (evalArithExpr e1 run, evalArithExpr e2 run) of
        (Just n1, Just n2)  -> (/=) n1 n2
        _                   -> error "evalPred: predicate not evaluatable!"         -- TODO: failure!
evalPred (PBtwn e1 e2 e3) run =
    case (evalArithExpr e1 run, evalArithExpr e2 run, evalArithExpr e3 run) of
        (Just n1, Just n2, Just n3) -> (&&) ((<=) n1 n2) ((<=) n2 n3)       -- n1 <= n2 <= n3
        _                           -> error "evalPred: predicate not evaluatable!"
evalPred (PLt e1 e2) run =
    case (evalArithExpr e1 run, evalArithExpr e2 run) of
        (Just n1, Just n2)  -> (<=) n1 n2
        _                   -> error "evalPred: predicate not evaluatable!"



-- evaluation : see AbstractExpr -> ConcreteExpr in FP course
eval :: Environment -> AbstractStrategy -> ConcreteStrategy
eval env (Do label) = \run ->
    case label of
        LDelay time -> Delay time
        _           -> 
            let resolvedLabel = resolveLabelID label env run in
                if executable resolvedLabel run
                    then Actions $ UnordSet [resolvedLabel]       -- resolve id in the label
                    -- else error $ "eval: " ++ show label ++ " is not executable!"
                    else eval env DoNothing run

eval env DoNothing = \run ->
    let minTime = minTimeRun run in
        eval env (WaitUntil minTime) run

eval _ (WaitUntil (Time d)) =
    \run -> let now = getCurrentTime run in
        if now < Time d 
            then Delay $ subTime (Time d) now
            else Delay TerminationTime          -- wait for a time in the past: Delay Termination
eval _ (WaitUntil TerminationTime) = \_ -> Delay TerminationTime


eval env (Combination as1 as2) = \run ->
    let cs1 = eval env as1 in               -- as1 and as2 are parallel evaluated
        let cs2 = eval env as2 in
            case (cs1 run, cs2 run) of
                (Delay t1, Delay t2)        -> Delay $ min t1 t2
                (Delay _, actionsList2)     -> actionsList2 
                (actionsList1, Delay _)     -> actionsList1
                (Actions s1, Actions s2)    ->
                    if isEmptySet s1                    -- possibly leads to Actions EmptySet => Delay min-time 
                        then Delay $ minTimeRun run
                        else Actions $ greedyActionsCombination s1 s2

eval env (ExecutedThenElse label succList as1 as2) = \run ->
    case label of
        LDelay _ -> error "eval ExecutedThenElse: LDelay cannot be applied here"
        _ -> let env' = updateAllSuccEnv label succList env in           -- (temp.) env': update env with label: (succ, index)
                if executedLabel (resolveLabelID label env run) run     -- resolve id in a label 
                    then eval env' as1 run                              -- only label executed: update env
                    else eval env as2 run

eval env (IfThenElse (BeforeTimeOut t) as1 as2) =            -- if before t then as1 else as2
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


--     print "Operators.hs"

