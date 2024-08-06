module Semantic.Environment (
    Environment
    , lookupEnv
    , updateEnv
) where

import Syntax.Common (Money (BCoins), Participant (Participant), ID (..), VarID, ConcID)
import Syntax.Contract (GuardedContract (Withdraw))
import Syntax.Label
import qualified Data.Map as Map        -- avoid name clashes



{- type for environment, alias of Map {id: (label, index)}
    label: predecessor label, defined in Syntax.Label
    index: decendent number under a predecessor label (LSplit)
-}
type Environment = Map.Map VarID (Label ID, Int)   -- alias, env = Map {vid: (label cid|vid, index)}


{- lookup id in environment
    if exist:   return Just (label ID, index)
    else:       return Nothing
-}
lookupEnv :: VarID -> Environment -> Maybe (Label ID, Int)
lookupEnv = Map.lookup



{- update the environment
    if id exist:    TODO
    else:           insert id: value into env
-}
updateEnv :: VarID -> (Label ID, Int) -> Environment -> Environment
updateEnv id (label, index) env =
    -- -- method 1: fail
    -- case lookupEnv id env of
    --     Just _  -> error "updateEnv: cannot update the environment with existing ID!"
    --     Nothing -> Map.insert id (label, index) env
    -- method 2: default update
    Map.insert id (label, index) env


{- create empty environment
    Map.empty
-}
emptyEnv :: Environment
emptyEnv = Map.empty


{- set an enviroment from a list of (k, v)
    []:     Map.empty
    else:   Map.Map
-}
setFromListEnv :: [(VarID, (Label ID, Int))] -> Environment
setFromListEnv = Map.fromList


-- main = do
--     let envEmpty = emptyEnv
--     -- print envEmpty 
--     let env01 = updateEnv (ID "x1") (LSplit (ID "x") (Withdraw (Participant "A")), 0) envEmpty
--     let env02 = setFromListEnv [(ID "y1", (LWithdraw (Participant "A") (BCoins 2) (ID "x"), 0))]
--     print $ lookupEnv (ID "x1") env01
--     print $ lookupEnv (ID "x1") env02