{-|
Module      : Syntax.Run
Description : Defines BitML run, config-objects and configuration 
-}

module Syntax.Run (
    ConfigObject (..)
    , Configuration
    , InitTime (..)
    , Run (..)
    , removeFirstList
    , removeLastList
    , transformRun
) where
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


removeFirstList :: [a] -> [a]
removeFirstList [] = []
removeFirstList (_ : xs) = xs


removeLastList :: [a] -> [a]
removeLastList []       = []
removeLastList [_]      = []
removeLastList (x : xs) = removeLastList xs ++ [x] 



{- transform run into {label1: ((initConf, t0), (conf1, t1)), ... labelj: ((confi, ti), (congj, tj))...}
-}
transformRun :: Run -> Map.Map (Label ConcID) ((Configuration, Time), (Configuration, Time))
transformRun (Run ((initConf, _), tplList@((label1, confList1, time1) : xsi)))  =
    case length tplList of
        0   -> Map.empty
        1   -> fstPairDict
        _   ->
            let configsList = zip [(c, t) | (l, c, t) <- removeFirstList xsi] [(c, t) | (l, c, t) <- removeLastList xsi] in     -- [((bf_conf, t), (aft_conf, t'))] 
                let labelConfigsList = zip [l | (l, _, _) <- removeFirstList xsi] configsList in                                -- [(label, ((confi, t1), (confj, tj)))]
                    foldl (\acc (label, confTupel) -> Map.insert label confTupel acc) fstPairDict labelConfigsList
    where fstPairDict = Map.insert label1 ((initConf, Time 0), (confList1, time1)) Map.empty        -- {label1: ((initConf, Time 0), (confList1, time1))}