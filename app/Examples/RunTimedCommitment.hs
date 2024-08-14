module Examples.RunTimedCommitment (
    contractTimedCommitment
    , run_tc_0
    , runTCReva
    , runTCPuta
    , runTCWithdrawB
) where

import Syntax.Run
import Syntax.Common
import Syntax.Contract
import Syntax.Label

contractTimedCommitment :: [GuardedContract]
contractTimedCommitment = [
    PutReveal [] [Secret "a"] PTrue [Withdraw (Participant "A")]
    , After (Time 2) (Withdraw (Participant "B"))
    ]

run_tc_0 :: Run
run_tc_0 = Run (([ActiveContract contractTimedCommitment (BCoins 1) (ConcID "tc")], InitTime), [])

runTCReva :: Run
runTCReva = Run (
                    ([ActiveContract contractTimedCommitment (BCoins 1) (ConcID "tc")], InitTime)
                    , [
                        (LAuthReveal (Participant "A") (Secret "a")
                        , [ActiveContract [PutReveal [] [Secret "a"] PTrue [Withdraw (Participant "A")]] (BCoins 1) (ConcID "tc")
                            , RevealedSecret (Participant "A") (Secret "a") 10]
                        , Time 0)
                    ]
                )

runTCPuta :: Run
runTCPuta = Run (
                    ([ActiveContract contractTimedCommitment (BCoins 1) (ConcID "tc")], InitTime)
                    , [
                        (LAuthReveal (Participant "A") (Secret "a")
                        , [ActiveContract contractTimedCommitment (BCoins 1) (ConcID "tc")
                            , RevealedSecret (Participant "A") (Secret "a") 10]
                        , Time 0)
                        , (LPutReveal [] [Secret "a"] PTrue (ConcID "tc") [Withdraw (Participant "A")]
                            , [ActiveContract [Withdraw (Participant "A")] (BCoins 1) (ConcID "x")
                                , RevealedSecret (Participant "A") (Secret "a") 10]
                            , Time 0
                        )
                    ]
                )

runTCWithdrawB :: Run
runTCWithdrawB = Run (([ActiveContract contractTimedCommitment (BCoins 1) (ConcID "tc")], InitTime)
            , [(LDelay (Time 2), [ActiveContract contractTimedCommitment (BCoins 1) (ConcID "tc")], Time 2)])
