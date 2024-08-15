module Examples.ZeroCollateralLottery.ZCLStrategies (
    paZCLStrategy
) where

import Examples.ZeroCollateralLottery.ZCLContract
import Syntax.Strategy
import Syntax.Common
import Syntax.Label

{- participant A's honest strategy -}
paZCLStrategy :: AbstractStrategy
paZCLStrategy = 
    IfThenElse (BeforeTimeOut timeD)
        (ExecutedThenElse (LAuthReveal (Participant "PB") (Secret "b")) []
            (IfThenElse (Predicate (PBtwn (EInt 0) (ELength (Secret "b")) (EInt 1)))
                (ExecutedThenElse (LPutReveal [] [Secret "b"] (PBtwn (EInt 0) (ELength (Secret "b")) (EInt 1)) (CID (ConcID "zcl")) zclCAftRevb) [VarID "x"] 
                    (ExecutedThenElse (LAuthReveal (Participant "PA") (Secret "a")) [] 
                        (IfThenElse (Predicate (PEq (ELength (Secret "a")) (ELength (Secret "b")))) 
                            (ExecutedThenElse (LPutReveal [] [Secret "a"] (PEq (ELength (Secret "a")) (ELength (Secret "b"))) (VID (VarID "x")) [zclGRevaEq]) [VarID "y"] 
                                (ExecutedThenElse (LWithdraw (Participant "PA") (BCoins 2) (VID (VarID "y"))) [] 
                                    DoNothing 
                                    (Do (LWithdraw (Participant "PA") (BCoins 2) (VID (VarID "y"))))
                                )
                                (Do (LPutReveal [] [Secret "a"] (PEq (ELength (Secret "a")) (ELength (Secret "b"))) (VID (VarID "x")) [zclGRevaEq]))
                            ) 
                            DoNothing
                        ) 
                        (Do (LAuthReveal (Participant "PA") (Secret "a")))
                    ) 
                    (Do (LPutReveal [] [Secret "b"] (PBtwn (EInt 0) (ELength (Secret "b")) (EInt 1)) (CID (ConcID "zcl")) zclCAftRevb))
                )
                DoNothing
            )
            DoNothing
        )
        (ExecutedThenElse (LWithdraw (Participant "PA") (BCoins 2) (CID (ConcID "zcl"))) [] 
            DoNothing
            (Do (LWithdraw (Participant "PA") (BCoins 2) (CID (ConcID "zcl"))))
        )
