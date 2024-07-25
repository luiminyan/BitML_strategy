{-|
Module      : Syntax.Run
Description : Defines BitML run, config-objects and configuration 
-}

module Syntax.Run where

import Syntax.Label (Label)
import Syntax.Common ( Secret, Money, Participant, ID, Time )
import Syntax.Contract (Contract, GuardedContract)


data ConfigObject =
    ActiveContract Contract Money ID 
    | Deposit Participant Money ID
    | AuthTakeBranch Participant ID GuardedContract     -- Authorization
    | SecretCommit Participant Secret
    | RevealedSecrect  Participant Secret Int
    deriving (Show)


type Configuration = [ConfigObject]

data InitConfiguration = InitConfig deriving (Show)

newtype Run = Run (InitConfiguration, [(Label, Configuration, Time)]) deriving (Show)