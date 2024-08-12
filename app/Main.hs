module Main where
    
import Examples.RunTimedCommitment 
import Examples.TimedCommitment
import Semantic.Operators
import Semantic.Environment 

main:: IO ()
main = do
    let a_strategy_tc = pa_timed_commitment
    
    let run_tc_0 = run_tc_0
    print $ eval emptyEnv single_do_nothing run_tc_0
    print "Main: test"