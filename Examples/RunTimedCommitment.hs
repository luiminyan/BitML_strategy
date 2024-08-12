module Examples.RunTimedCommitment (
    contract_timed_commitment
    , run_tc_0
    , run_tc_rev_a
    , run_tc_withdraw_B
) where

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
                (LAuthReveal (Participant "A") (Secret "a"), [ActiveContract [Withdraw (Participant "A")] (BCoins 1) (ConcID "tc"), RevealedSecret (Participant "A") (Secret "a") 10], Time 0)
            ])
run_tc_put_a = Run (([ActiveContract contract_timed_commitment (BCoins 1) (ConcID "tc")], InitTime)
            , [
                (LAuthReveal (Participant "A") (Secret "a"), [ActiveContract [PutReveal [] [Secret "a"] PTrue [Withdraw (Participant "A")]] (BCoins 1) (ConcID "tc"), RevealedSecret (Participant "A") (Secret "a") 10], Time 0)
                , (LPutReveal [] [Secret "a"] PTrue (ConcID "tc'") (Withdraw (Participant "A")), [], Time 1)
            ])
run_tc_withdraw_B = Run (([ActiveContract contract_timed_commitment (BCoins 1) (ConcID "tc")], InitTime)
            , [(LWithdraw (Participant "B") (BCoins 1) (ConcID "tc_b"), [Deposit (Participant "B") (BCoins 1) (ConcID "depo_B")], Time 2)])
