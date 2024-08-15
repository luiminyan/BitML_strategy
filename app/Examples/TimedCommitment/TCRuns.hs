module Examples.TimedCommitment.TCRuns (
    run_tc_0
    , runTCReva
    , runTCPuta
    , runTCWithdrawB
) where

import Syntax.Run
import Syntax.Common
import Syntax.Contract
import Syntax.Label
import Examples.TimedCommitment.TCContract



run_tc_0 :: Run
run_tc_0 = Run ([ActiveContract contractTimedCommitment (BCoins 1) (ConcID "tc")], [])

runTCReva :: Run
runTCReva = appendRun (LAuthReveal (Participant "A") (Secret "a")) [ActiveContract [PutReveal [] [Secret "a"] PTrue [Withdraw (Participant "A")]] (BCoins 1) (ConcID "tc"), RevealedSecret (Participant "A") (Secret "a") 10] (Time 0) run_tc_0


runTCPuta :: Run
runTCPuta = appendRun (LPutReveal [] [Secret "a"] PTrue (ConcID "tc") [Withdraw (Participant "A")]) [ActiveContract [Withdraw (Participant "A")] (BCoins 1) (ConcID "x"), RevealedSecret (Participant "A") (Secret "a") 10] (Time 0) runTCReva


runTCWithdrawB :: Run
runTCWithdrawB = Run ([ActiveContract contractTimedCommitment (BCoins 1) (ConcID "tc")], [(LDelay (Time 2), [ActiveContract contractTimedCommitment (BCoins 1) (ConcID "tc")], Time 2)])
