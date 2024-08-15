{-|
Module      : Syntax.Run
Description : Defines BitML run, config-objects and configuration 
-}

module Syntax.Run (
    ConfigObject (..)
    , Configuration
    , Run (..)
    , transformRun
    , searchTransRun
    , appendRun
) where
import Syntax.Label (Label)
import Syntax.Common ( Secret, Money, Participant, ConcID, Time(..) )
import Syntax.Contract (Contract, GuardedContract)
import Data.List (find)


data ConfigObject =
    ActiveContract Contract Money ConcID
    | Deposit Participant Money ConcID
    | AuthTakeBranch Participant ConcID GuardedContract     -- Authorization
    | SecretCommit Participant Secret
    | RevealedSecret Participant Secret Int
    deriving (Eq, Show)


type Configuration = [ConfigObject]


newtype Run = Run (Configuration, [(Label ConcID, Configuration, Time)]) deriving (Eq, Show)    -- Run (InitConfig, [(label, ConfigList, time)])     



{- transform run into a list [(label1, ((initConf, t0), (conf1, t1))), ... (labelj, ((confi, ti), (congj, tj)))...]
    init-run (no label executed), return []
-}
transformRun :: Run -> [(Label ConcID, ((Configuration, Time), (Configuration, Time)))]
transformRun (Run (_, [])) = []
transformRun (Run (initConf, tplList@((label1, confList1, time1) : xsi)))  =
    case length tplList of
        1   -> fstPairList
        _   ->
            let configsList = zip [(c, t) | (_, c, t) <- init tplList] [(c, t) | (_, c, t) <- xsi] in     -- (noLast, noHead)    [((bf_conf, t), (aft_conf, t'))] 
                let labelConfigsList = zip [l | (l, _, _) <- xsi] configsList in                                -- [(label, ((confi, t1), (confj, tj)))]
                    foldl (\acc labelConfTupel -> acc ++ [labelConfTupel]) fstPairList labelConfigsList
    where fstPairList = [(label1, ((initConf, Time 0), (confList1, time1)))]        -- {label1: ((initConf, Time 0), (confList1, time1))}




{- search for a label in the transformed-run
    exist: return the first found item ((ConfBF, t), (ConfAFT, t'))
    else: Nothing
-}
searchTransRun :: Label ConcID -> [(Label ConcID, ((Configuration, Time), (Configuration, Time)))] -> Maybe ((Configuration, Time), (Configuration, Time))
searchTransRun label transRun = fmap snd (find (\(l, _) -> label == l) transRun)


{- append run with new (label, Configuration, time)
-}
appendRun :: Label ConcID -> Configuration -> Time -> Run -> Run
appendRun label configuration time (Run (initConfig, tupulList)) = Run (initConfig, tupulList ++ [(label, configuration, time)])