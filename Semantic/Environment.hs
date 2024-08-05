module Semantic.Environment (
    Env
    , lookupEnv
    , updateEnv
) where

import Syntax.Common (ID (ID), Money (BCoins), Participant (Participant))
import Syntax.Contract (GuardedContract (Withdraw))
import Syntax.Label
import qualified Data.Map as Map        -- avoid name clashes



{- type for environment, alias of Map {id: (label, index)}
    label: predecessor label, defined in Syntax.Label
    index: decendent number under a predecessor label (LSplit)
-}
type Env = Map.Map ID (Label, Int)   -- alias, env = Map {id: (label, index)}


{- lookup id in environment
    if exist:   return Just (label, index)
    else:       return Nothing
-}
lookupEnv :: ID -> Env -> Maybe (Label, Int)
lookupEnv = Map.lookup



{- update the environment
    if id exist:    do nothing (cannot rewrite the predecessor)
    else:           insert id: value into env
-}
updateEnv :: ID -> (Label, Int) -> Env -> Env
updateEnv id (label, index) env =
    case lookupEnv id env of
        Just _  -> env
        Nothing -> Map.insert id (label, index) env


{- create empty environment
    Map.empty
-}
emptyEnv :: Env
emptyEnv = Map.empty


{- set an enviroment from a list of (k, v)
    []:     Map.empty
    else:   Map.Map
-}
setFromListEnv :: [(ID, (Label, Int))] -> Env
setFromListEnv = Map.fromList


-- main = do
--     let envEmpty = emptyEnv
--     -- print envEmpty 
--     let env01 = updateEnv (ID "x1") (LSplit (ID "x") (Withdraw (Participant "A")), 0) envEmpty
--     let env02 = setFromListEnv [(ID "y1", (LWithdraw (Participant "A") (BCoins 2) (ID "x"), 0))]
--     print $ lookupEnv (ID "x1") env01
--     print $ lookupEnv (ID "x1") env02