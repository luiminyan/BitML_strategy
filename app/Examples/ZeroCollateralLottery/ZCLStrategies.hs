module Examples.ZeroCollateralLottery.ZCLStrategies (
    paZCLStrategy
    , pbZCLStrategy
) where

import Examples.ZeroCollateralLottery.ZCLContract
import Syntax.Strategy
import Syntax.Common
import Syntax.Label


{- participant A's honest strategy -}
paZCLStrategy :: AbstractStrategy
paZCLStrategy = 
    IfThenElse (BeforeTimeOut timeD)
        (ExecutedThenElse (LAuthReveal pb sec_b) []
            (IfThenElse (Predicate (PBtwn (EInt 0) (ELength sec_b) (EInt 1)))
                (ExecutedThenElse (LPutReveal [] [sec_b] (PBtwn (EInt 0) (ELength sec_b) (EInt 1)) (CID (ConcID "zcl")) zclCAftRevb) [VarID "x"] 
                    (ExecutedThenElse (LAuthReveal pa sec_a) [] 
                        (IfThenElse (Predicate (PEq (ELength sec_a) (ELength sec_b))) 
                            (ExecutedThenElse (LPutReveal [] [sec_a] (PEq (ELength sec_a) (ELength sec_b)) (VID (VarID "x")) [zclGWithdrawA]) [VarID "y"] 
                                (ExecutedThenElse (LWithdraw pa (BCoins 2) (VID (VarID "y"))) [] 
                                    DoNothing 
                                    (Do (LWithdraw pa (BCoins 2) (VID (VarID "y"))))
                                )
                                (Do (LPutReveal [] [sec_a] (PEq (ELength sec_a) (ELength sec_b)) (VID (VarID "x")) [zclGWithdrawA]))
                            ) 
                            DoNothing
                        ) 
                        (Do (LAuthReveal pa sec_a))
                    ) 
                    (Do (LPutReveal [] [sec_b] (PBtwn (EInt 0) (ELength sec_b) (EInt 1)) (CID (ConcID "zcl")) zclCAftRevb))
                )
                DoNothing
            )
            DoNothing
        )
        (ExecutedThenElse (LWithdraw pa (BCoins 2) (CID (ConcID "zcl"))) [] 
            DoNothing
            (Do (LWithdraw pa (BCoins 2) (CID (ConcID "zcl"))))
        )


{- participant B's honest strategy -}
pbZCLStrategy :: AbstractStrategy
pbZCLStrategy = 
    IfThenElse (BeforeTimeOut timeD)
        (ExecutedThenElse (LAuthReveal pb sec_b) []
            (IfThenElse (Predicate (PBtwn (EInt 0) (ELength sec_b) (EInt 1)))
                (ExecutedThenElse (LPutReveal [] [sec_b] (PBtwn (EInt 0) (ELength sec_b) (EInt 1)) (CID (ConcID "zcl")) zclCAftRevb) [VarID "vx"]
                    (ExecutedThenElse (LAuthReveal pa sec_a) []
                        (IfThenElse (Predicate (PEq (ELength sec_a) (ELength sec_b)))
                            (ExecutedThenElse (LPutReveal [] [sec_a] (PNeq (ELength sec_a) (ELength sec_b)) (VID (VarID "vx")) [zclGWithdrawB]) [VarID "vy"]
                                (ExecutedThenElse (LWithdraw pb (BCoins 2) (VID (VarID "vy"))) []
                                    (WaitUntil timeD')
                                    (Do (LWithdraw pb (BCoins 2) (VID (VarID "vy"))))
                                )
                                (Do (LPutReveal [] [sec_a] (PNeq (ELength sec_a) (ELength sec_b)) (VID (VarID "vx")) [zclGWithdrawB]))
                            ) 
                            (WaitUntil timeD')
                        )
                        (WaitUntil timeD')
                    )
                    (Do (LPutReveal [] [sec_b] (PBtwn (EInt 0) (ELength sec_b) (EInt 1)) (CID (ConcID "zcl")) zclCAftRevb))
                )
                (WaitUntil timeD')    
            )
            (Do (LAuthReveal pb sec_b))
        )
        (IfThenElse (BeforeTimeOut timeD')
            (WaitUntil timeD')
            (ExecutedThenElse (LAuthReveal pb sec_b) []
                (ExecutedThenElse (LPutReveal [] [sec_b] (PBtwn (EInt 0) (ELength sec_b) (EInt 1)) (CID (ConcID "zcl")) zclCAftRevb) [VarID "vx"]
                    (ExecutedThenElse (LWithdraw pb (BCoins 2) (VID (VarID "vx"))) []
                        DoNothing
                        (Do (LWithdraw pb (BCoins 2) (VID (VarID "vx"))))
                    )
                    (Do (LPutReveal [] [sec_b] (PBtwn (EInt 0) (ELength sec_b) (EInt 1)) (CID (ConcID "zcl")) zclCAftRevb))
                )
                (Do (LAuthReveal pb sec_b))
            )
        ) 