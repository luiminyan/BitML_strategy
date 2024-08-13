module Main (main) where

import Examples.RunTimedCommitment
import Examples.TimedCommitment
import Semantic.Operators
import Semantic.Environment

main :: IO ()
main = do
    print $ eval emptyEnv singleDoNothing run_tc_0
    print $ eval emptyEnv singleDoLabel run_tc_0
    print $ eval emptyEnv paTimedCommitment runTCWithdrawB
    print $ eval emptyEnv paTimedCommitment run_tc_0
    print $ eval emptyEnv paTimedCommitment runTCReva
    print $ eval emptyEnv paTimedCommitment runTCPuta


    print "Main: test"