module Examples.RockPaperScissors.RPSStrategies (
    paRpsStrategy
) where
import Syntax.Strategy
import Examples.RockPaperScissors.RPSContract
import Syntax.Label
import Syntax.Common



paRpsStrategy :: AbstractStrategy
paRpsStrategy = ExecutedThenElse (LSplit (CID (ConcID "rps")) [(BCoins 2, rpsCon0), (BCoins 2, rpsCon1), (BCoins 2, rpsCon2)]) [VarID "x0", VarID "x1", VarID "x2"]
    (Combination (Combination paRpsStrategy0 paRpsStrategy1) paRpsStrategy2)
    (Do (LSplit (CID (ConcID "rps")) [(BCoins 2, rpsCon0), (BCoins 2, rpsCon1), (BCoins 2, rpsCon2)]))


paRpsStrategy0 :: AbstractStrategy
paRpsStrategy0 = IfThenElse (BeforeTimeOut timeD)
    (WaitUntil timeD)
    (ExecutedThenElse (LWithdraw pa (BCoins 2) (VID (VarID "x0"))) []
        DoNothing
        (Do (LWithdraw pa (BCoins 2) (VID (VarID "x0"))))
    )


paRpsStrategy1 :: AbstractStrategy
paRpsStrategy1 = IfThenElse (BeforeTimeOut timeD)
    (ExecutedThenElse (LAuthReveal pa secA) []
        (IfThenElse (Predicate (PBtwn (EInt 0) (ELength secA) (EInt 2)))
            (ExecutedThenElse (LPutReveal [] [secA] (PBtwn (EInt 0) (ELength secA) (EInt 2)) (VID (VarID "x1")) [gConWithdrawPA]) [VarID "x1*"]
                (ExecutedThenElse (LWithdraw pa (BCoins 2) (VID (VarID "x1*"))) []
                    DoNothing
                    (Do (LWithdraw pa (BCoins 2) (VID (VarID "x1*"))))
                )
                (Do (LPutReveal [] [secA] (PBtwn (EInt 0) (ELength secA) (EInt 2)) (VID (VarID "x1")) [gConWithdrawPA]))
            )
            DoNothing
        )
        (Do (LAuthReveal pa secA))
    )
    DoNothing


paRpsStrategy2 :: AbstractStrategy
paRpsStrategy2 =
    ExecutedThenElse (LAuthReveal pa secA) []
        (ExecutedThenElse (LAuthReveal pb secB) []
            (IfThenElse (Predicate (PLt (ELength secB) (ELength secA)))
                (ExecutedThenElse (LPutReveal [] [secA, secB] (win secA secB) (VID (VarID "x3")) [gConWithdrawPA]) [VarID "x3*"]
                    (ExecutedThenElse (LWithdraw pa (BCoins 2) (VID (VarID "x3*"))) []
                        DoNothing
                        (Do (LWithdraw pa (BCoins 2) (VID (VarID "x3*"))))
                    )
                    (Do (LPutReveal [] [secA, secB] (win secA secB) (VID (VarID "x3")) [gConWithdrawPA]))
                )
                (IfThenElse (Predicate (PEq (ELength secA) (ELength secB)))
                    (ExecutedThenElse (LPutReveal [] [secA, secB] (PEq (ELength secA) (ELength secB)) (VID (VarID "x3")) rpsCon2Split) [VarID "x3**"]
                        (ExecutedThenElse (LSplit (VID (VarID "x3**")) [(BCoins 1, [gConWithdrawPA]), (BCoins 1, [gConWithdrawPB])]) [VarID "x4", VarID "x5"]
                            (ExecutedThenElse (LWithdraw pa (BCoins 1) (VID (VarID "x4"))) []
                                DoNothing
                                (Do (LWithdraw pa (BCoins 1) (VID (VarID "x4"))))
                            )
                            (Do (LSplit (VID (VarID "x3**")) [(BCoins 1, [gConWithdrawPA]), (BCoins 1, [gConWithdrawPB])]))
                        )
                        (Do (LPutReveal [] [secA, secB] (PEq (ELength secA) (ELength secB)) (VID (VarID "x3")) rpsCon2Split))
                    )
                    DoNothing
                )
            )
            DoNothing       -- cannot reveal secrect from PB
        )
        (Do (LAuthReveal pa secA))
