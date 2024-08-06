module Semantic.Operators where
import Syntax.Label
import Syntax.Common (ID (..), Participant (Participant), Money (BCoins), Pred (..), Secret (Secret), Time (..), E (..), subTime, ConcID, VarID (..), ConcID (..))
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
        unionSet s1 (unionSet s2_empty s2_unique)



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




{- get the length of a secret
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


{- TODO: resolve VarID into ConcID
    search for the predecessor of VarID in Environment till a ConcID
    replace VarID with the corresponding ConcID in run
-}  
resolveID :: ID -> Environment -> Run -> ConcID
resolveID (CID concId) _ _ = concId 
resolveID (VID (VarID id)) _ _ = ConcID id

{- TODO for eval
    1. update eval with 'env'
    2. update 'do label' with 'resolve id'

    *. finish IfElseThen predicate
-}


-- evaluation : see AbstractExpr -> ConcreteExpr in FP course
eval :: Environment -> AbstractStrategy -> ConcreteStrategy
eval env (Do label) = \run -> 
    case label of
        LSplit id gc -> Actions $ UnordSet [LSplit (resolveID id env run) gc]
        LAuthReveal p s -> Actions $ UnordSet [LAuthReveal p s]
        LPutReveal ds secs p id gc -> Actions $ UnordSet [LPutReveal ds secs p (resolveID id env run) gc]
        LWithdraw p m id -> Actions $ UnordSet [LWithdraw p m (resolveID id env run)]
        LAuthControl p id gc -> Actions $ UnordSet [LAuthControl p (resolveID id env run) gc]

        
eval env DoNothing = Delay . minTimeRun         -- DoNothing = delay minimum time from active contract(s) in run | termination-time

eval env (WaitUntil (Time d)) =
    \run -> let now = getCurrentTime run in
        if now < Time d then Delay $ subTime (Time d) now
        else error "Invalid strategy: time to wait already past"

eval env (Combination as1 as2) = \run ->
    let cs1 = eval env as1 in               -- cs1 run = sr1
        let cs2 = eval env as2 in
            case (cs1 run, cs2 run) of
                (Delay t1, Delay t2)        -> Delay $ min t1 t2
                (Delay t1, as2)             -> as2
                (as1, Delay t2)             -> as1
                (Actions s1, Actions s2)    -> Actions $ greedyActionsCombination s1 s2

eval env (IfThenElse (CheckTimeOut t) as1 as2) =
    \run -> let cs1 = eval env as1 run in
        let cs2 = eval env as2 run in
            let now = getCurrentTime run in
                if t < now then cs1 else cs2

-- TODO: finish if-else-then predicate
eval env (IfThenElse (Predicate PTrue) as1 as2) = eval env as1







main = do
    -- test cv
    -- let l1 = LWithdraw (Participant "A") (BCoins 1) (ID "x1")
    -- let l2 = LAuthControl (Participant "A") (ID "x1") (Withdraw (Participant "A"))
    -- let l3 = LSplit (ID "x1")
    -- print $ cv l1       -- Just [ID "x1"]
    -- print $ cv l2       -- Nothing

    -- test greedyActionsCombination
    -- let s1 = insertList EmptySet [LWithDraw (Participant "A") (BCoins 1) (ID "x1"), LAuthControl (Participant "A") (ID "x1") (Withdraw (Participant "A"))]
    -- let s4 = insertList EmptySet [LSplit (ID "x1"), LPutReveal [] [] PTrue (ID "x1"), LAuthControl (Participant "A") (ID "x1") (Withdraw (Participant "A")), LAuthReveal (Participant "A") (Secret "a")]
    -- print "Operations.hs"
    -- print $ greedyActionsCombination s1 s4


    -- TODO: test minTimeRun
    -- print $ minTimeG (Split [])   not allowed by BitML semantics
    print "Operators.hs"


