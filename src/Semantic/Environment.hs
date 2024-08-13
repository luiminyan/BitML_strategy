module Semantic.Environment (
    Environment
    , lookupEnv
    , updateEnv
    , emptyEnv
    , envToList
    , setFromListEnv
) where

import Syntax.Common 
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



{- update the environment with put or split label and its' successor 
    with protections against:
    1. rewrite existing successor
    2. Environment loop: add successor which already exists in env as predecessor (VarID)
-}
updateEnv :: VarID -> (Label ID, Int) -> Environment -> Environment
updateEnv succId (label, index) env =
    case lookupEnv succId env of
        Just _  -> error "updateEnv: successor ID already exist!"
        Nothing ->
            let predLabelList = filter putOrSplit $ map (fst . snd) $ Map.toList env in
                let predIDList = concatMap cv predLabelList in
                    let varIDList = [vid | (VID vid) <- predIDList] in          -- extract all var id from list
                        if elem succId varIDList 
                            then error "updateEnv: Environment loop" 
                            else Map.insert succId (label, index) env




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


{- return a list = [(key, value)] 
-}
envToList :: Environment -> [(VarID, (Label ID, Int))]
envToList = Map.toList

