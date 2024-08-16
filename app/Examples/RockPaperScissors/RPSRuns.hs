module Examples.RockPaperScissors.RPSRuns (
    runRPS0
    , runRPSSplit
    , runRPSAftD
    , runRPSWDrawA0
    , runRPSRevSecA
    , runRPSPutA
    , runRPSWDrawA1
    , runRPSRevB
    , runRPSPutABWinA
    , runRPSWDrawA2WinA
    , runRPSPutABDraw
    , runRPS2Split
    , runRPS3WDrawA
) where

import Syntax.Common
import Syntax.Run
import Syntax.Label
import Examples.RockPaperScissors.RPSContract

lenSeca :: Int
lenSeca = 1

lenSecb :: Int
lenSecb = 0


runRPS0 :: Run
runRPS0 = Run ([ActiveContract rpsContract (BCoins 6) (ConcID "rps")], [])


runRPSSplit :: Run
runRPSSplit = 
    appendRun 
        (LSplit (ConcID "rps") [(BCoins 2, rpsCon0), (BCoins 2, rpsCon1), (BCoins 2, rpsCon2)]) 
        [
            ActiveContract rpsCon0 (BCoins 2) (ConcID "conc-x0")
            , ActiveContract rpsCon1 (BCoins 2) (ConcID "conc-x1")
            , ActiveContract rpsCon2 (BCoins 2) (ConcID "conc-x2")
        ] 
        (Time 0) 
        runRPS0



runRPSAftD :: Run
runRPSAftD = 
    appendRun 
        (LDelay timeD)
        [
            ActiveContract rpsCon0 (BCoins 2) (ConcID "conc-x0")
            , ActiveContract rpsCon1 (BCoins 2) (ConcID "conc-x1")
            , ActiveContract rpsCon2 (BCoins 2) (ConcID "conc-x2")
        ]
        timeD
        runRPSSplit


-- init -> split -> WDraw: WaitUntil D
runRPSWDrawA0 :: Run
runRPSWDrawA0 = 
    appendRun 
        (LWithdraw pa (BCoins 2) (ConcID "conc-x0")) 
        [
            Deposit pa (BCoins 2) (ConcID "depo-A-0")
            , ActiveContract rpsCon1 (BCoins 2) (ConcID "conc-x1")
            , ActiveContract rpsCon2 (BCoins 2) (ConcID "conc-x2")
        ] 
        timeD
        runRPSAftD
        


runRPSRevSecA :: Run
runRPSRevSecA = 
    appendRun 
        (LAuthReveal pa secA) 
        [
            ActiveContract rpsCon0 (BCoins 2) (ConcID "conc-x0")
            , ActiveContract rpsCon1 (BCoins 2) (ConcID "conc-x1")
            , ActiveContract rpsCon2 (BCoins 2) (ConcID "conc-x2")
            , RevealedSecret pa secA lenSeca
        ] 
        (Time 0) 
        runRPSSplit 


runRPSPutA :: Run
runRPSPutA = 
    appendRun 
        (LPutReveal [] [secA] (PBtwn (EInt 0) (ELength secA) (EInt 2)) (ConcID "conc-x1") [gConWithdrawPA]) 
        [
            ActiveContract rpsCon0 (BCoins 2) (ConcID "conc-x0")
            , ActiveContract [gConWithdrawPA] (BCoins 2) (ConcID "conc-x1-aftPutA")
            , ActiveContract rpsCon2 (BCoins 2) (ConcID "conc-x2")
            , RevealedSecret pa secA lenSeca
        ] 
        (Time 0) 
        runRPSRevSecA


runRPSWDrawA1 :: Run
runRPSWDrawA1 = 
    appendRun 
        (LWithdraw pa (BCoins 2) (ConcID "conc-x1-aftPutA")) 
        [
            ActiveContract rpsCon0 (BCoins 2) (ConcID "conc-x0")
            , Deposit pa (BCoins 2) (ConcID "depo-A-1")
            , ActiveContract rpsCon2 (BCoins 2) (ConcID "conc-x2")
            , RevealedSecret pa secA lenSeca
        ] 
        (Time 0) 
        runRPSPutA



runRPSRevB :: Run
runRPSRevB =  
    appendRun 
        (LAuthReveal pb secB) 
        [
            ActiveContract rpsCon0 (BCoins 2) (ConcID "conc-x0")
            , ActiveContract rpsCon1 (BCoins 2) (ConcID "conc-x1")
            , ActiveContract rpsCon2 (BCoins 2) (ConcID "conc-x2")
            , RevealedSecret pa secA lenSeca
            , RevealedSecret pb secB lenSecb
        ] 
        (Time 0) 
        runRPSRevSecA 


runRPSPutABWinA :: Run
runRPSPutABWinA =  
    appendRun 
        (LPutReveal [] [secA, secB] (win secA secB) (ConcID "conc-x2") [gConWithdrawPA]) 
        [
            ActiveContract rpsCon0 (BCoins 2) (ConcID "conc-x0")
            , ActiveContract rpsCon1 (BCoins 2) (ConcID "conc-x1")
            , ActiveContract [gConWithdrawPA] (BCoins 2) (ConcID "conc-x2-aftPutAB-WinA")
            , RevealedSecret pa secA lenSeca
            , RevealedSecret pb secB lenSecb
        ] 
        (Time 0) 
        runRPSRevB


runRPSWDrawA2WinA :: Run
runRPSWDrawA2WinA = 
    appendRun 
        (LWithdraw pa (BCoins 2) (ConcID "conc-x2-aftPutAB-WinA")) 
        [
            ActiveContract rpsCon0 (BCoins 2) (ConcID "conc-x0")
            , ActiveContract rpsCon1 (BCoins 2) (ConcID "conc-x1")
            , Deposit pa (BCoins 2) (ConcID "depo-A-2-win")
            , RevealedSecret pa secA lenSeca
            , RevealedSecret pb secB lenSecb
        ] 
        (Time 0) 
        runRPSPutABWinA


runRPSPutABDraw :: Run
runRPSPutABDraw = 
    appendRun 
        (LPutReveal [] [secA, secB] (PEq (ELength secA) (ELength secB)) (ConcID "conc-x2") rpsCon2Split)
        [
            ActiveContract rpsCon0 (BCoins 2) (ConcID "conc-x0")
            , ActiveContract rpsCon1 (BCoins 2) (ConcID "conc-x1")
            , ActiveContract rpsCon2Split (BCoins 2) (ConcID "conc-x2-aftPutAB-Draw")
            , RevealedSecret pa secA lenSeca
            , RevealedSecret pb secB lenSecb
        ] 
        (Time 0)
        runRPSRevB


runRPS2Split :: Run
runRPS2Split = 
    appendRun 
        (LSplit (ConcID "conc-x2-aftPutAB-Draw") [(BCoins 1, [gConWithdrawPA]), (BCoins 1, [gConWithdrawPB])]) 
        [
            ActiveContract rpsCon0 (BCoins 2) (ConcID "conc-x0")
            , ActiveContract rpsCon1 (BCoins 2) (ConcID "conc-x1")
            , ActiveContract [gConWithdrawPA] (BCoins 1) (ConcID "conc-x3")
            , ActiveContract [gConWithdrawPB] (BCoins 1) (ConcID "conc-x4")
            , RevealedSecret pa secA lenSeca
            , RevealedSecret pb secB lenSecb
        ] 
        (Time 0) 
        runRPSPutABDraw


runRPS3WDrawA :: Run
runRPS3WDrawA = 
    appendRun 
        (LWithdraw pa (BCoins 1) (ConcID "conc-x3")) 
        [
            ActiveContract rpsCon0 (BCoins 2) (ConcID "conc-x0")
            , ActiveContract rpsCon1 (BCoins 2) (ConcID "conc-x1")
            , Deposit pa (BCoins 1) (ConcID "depo-A-3")
            , ActiveContract [gConWithdrawPB] (BCoins 1) (ConcID "conc-x4")
            , RevealedSecret pa secA lenSeca
            , RevealedSecret pb secB lenSecb
        ] 
        (Time 0) 
        runRPS2Split