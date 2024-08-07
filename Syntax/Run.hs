{-|
Module      : Syntax.Run
Description : Defines BitML run, config-objects and configuration 
-}

module Syntax.Run where
import Syntax.Label (Label)
import Syntax.Common ( Secret, Money, Participant, ConcID, Time )
import Syntax.Contract (Contract, GuardedContract)


data ConfigObject =
    ActiveContract Contract Money ConcID 
    | Deposit Participant Money ConcID
    | AuthTakeBranch Participant ConcID GuardedContract     -- Authorization
    | SecretCommit Participant Secret
    | RevealedSecret  Participant Secret Int
    deriving (Show)


type Configuration = [ConfigObject]

data InitConfiguration = InitConfig deriving (Show)

newtype Run = Run (InitConfiguration, [(Label ConcID, Configuration, Time)]) deriving (Show)        -- Run can only have ConcID