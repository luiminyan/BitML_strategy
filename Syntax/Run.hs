{-|
Module      : Syntax.Run
Description : Defines BitML run, config-objects and configuration 
-}

module Syntax.Run where
import Syntax.Label (Label)
import Syntax.Common ( Secret, Money, Participant, ConcID, Time(..) )
import Syntax.Contract (Contract, GuardedContract)
import qualified Data.Map as Map


data ConfigObject =
    ActiveContract Contract Money ConcID 
    | Deposit Participant Money ConcID
    | AuthTakeBranch Participant ConcID GuardedContract     -- Authorization
    | SecretCommit Participant Secret
    | RevealedSecret  Participant Secret Int
    deriving (Show)


type Configuration = [ConfigObject]


data InitTime = InitTime deriving (Show)        -- initTime = Time 0


newtype Run = Run ((Configuration, InitTime), [(Label ConcID, Configuration, Time)]) deriving (Show)    -- Run (InitConfig, [(label, ConfigList, time)])     


transformRun :: Run -> Map.Map (Label ConcID) ((Configuration, Time), (Configuration, Time))
transformRun (Run ((initConf, _), tplList@((label, confList, t):xs)))  = 
    case length tplList of
        0   -> Map.empty
        1   -> Map.insert label ((initConf, Time 0), (confList, t)) Map.empty