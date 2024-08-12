module Main where
    
import Examples.RunTimedCommitment 
import Examples.TimedCommitment
import Semantic.Operators
import Semantic.Environment 


main = do
    print $ (eval emptyEnv single_do_nothing) run_tc_0
    print $ (eval emptyEnv single_do_label) run_tc_0
    print $ (eval emptyEnv pa_timed_commitment) run_tc_rev_a
    print $ (eval emptyEnv pa_timed_commitment) run_tc_0
    print "Main: test"