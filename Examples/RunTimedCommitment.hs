module Examples.RunTimedCommitment where

import Syntax.Run
import Syntax.Common
import Syntax.Contract
import Syntax.Label

contract_timed_commitment = [
    PutReveal [] [Secret "a"] PTrue [Withdraw (Participant "A")]
    , After (Time 2) (Withdraw (Participant "B"))
    ]

run_tc_0 = Run (([ActiveContract contract_timed_commitment (BCoins 1) (ConcID "tc")], InitTime), [])
run_tc_rev_a = Run (([ActiveContract contract_timed_commitment (BCoins 1) (ConcID "tc")], InitTime)
            , [
                (LAuthReveal (Participant "A") (Secret "a"), [ActiveContract [Withdraw (Participant "A")] (BCoins 1) (ConcID "tc")], Time 0)
            ])
run_tc_withdraw_B = Run (([ActiveContract contract_timed_commitment (BCoins 1) (ConcID "tc")], InitTime)
            , [(LWithdraw (Participant "B") (BCoins 1) (ConcID "tc_b"), [Deposit (Participant "B") (BCoins 1) (ConcID "depo_B")], Time 2)])
