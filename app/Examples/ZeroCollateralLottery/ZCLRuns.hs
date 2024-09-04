module Examples.ZeroCollateralLottery.ZCLRuns (
    zclRun0
    , zclRunRevb
    , zclRunPutb
    , zclRunReva
    , zclRunPuta
    , zclRunWithdrawA
    , zclRunPutANeq
    , zclRunWithdrawB
    , zclRunAfterD
    , zclRunAftDWDrawA
    , zclRunAfterD'
    , zclRunAftD'RevB
    , zclRunAftD'PutB
    , zclRunAftD'WDrawB
) where

import Examples.ZeroCollateralLottery.ZCLContract
import Syntax.Run
import Syntax.Common
import Syntax.Label


lenSeca :: Int
lenSeca = 1

lenSecb :: Int
lenSecb = 1

zclRun0 :: Run
zclRun0 = Run ([ActiveContract zclContract (BCoins 2) (ConcID "zcl")], [])

zclRunRevb :: Run
zclRunRevb = 
    appendRun 
        (LAuthReveal pb sec_b) 
        [
            ActiveContract zclContract (BCoins 2) (ConcID "zcl")
            , RevealedSecret pb sec_b lenSecb
        ] 
        (Time 0) 
        zclRun0


zclRunPutb :: Run
zclRunPutb = 
    appendRun 
        (LPutReveal [] [sec_b] (PBtwn (EInt 0) (ELength sec_b) (EInt 1)) (ConcID "zcl") zclCAftRevb) 
        [
            ActiveContract zclContract (BCoins 2) (ConcID "zcl-aftPutB")
            , RevealedSecret pb sec_b lenSecb
        ] 
        (Time 0) 
        zclRunRevb


zclRunReva :: Run
zclRunReva = 
    appendRun 
        (LAuthReveal pa sec_a) 
        [
            ActiveContract zclCAftRevb (BCoins 2) (ConcID "zcl-aftPutB")
            , RevealedSecret pb sec_b lenSecb, RevealedSecret pa sec_a lenSeca
        ] 
        (Time 0) 
        zclRunPutb


zclRunPuta :: Run
zclRunPuta = 
    appendRun 
        (LPutReveal [] [sec_a] (PEq (ELength sec_a) (ELength sec_b)) (ConcID "zcl-aftPutB") [zclGWithdrawA]) 
        [
            ActiveContract [zclGWithdrawA] (BCoins 2) (ConcID "zcl-aftPutA")
            , RevealedSecret pb sec_b lenSecb
            , RevealedSecret pa sec_a lenSeca
        ] 
        (Time 0) 
        zclRunReva


zclRunWithdrawA :: Run
zclRunWithdrawA = 
    appendRun 
        (LWithdraw pa (BCoins 2) (ConcID "zcl-aftPutA")) 
        [
            RevealedSecret pb sec_b lenSecb
            , RevealedSecret pa sec_a lenSeca
            , Deposit pa (BCoins 2) (ConcID "depo-pa-1")
        ] 
        (Time 0) 
        zclRunPuta

zclRunPutANeq :: Run
zclRunPutANeq = 
    appendRun 
        (LPutReveal [] [sec_a] (PNeq (ELength sec_a) (ELength sec_b)) (ConcID "zcl-aftPutB") [zclGWithdrawB]) 
        [
            ActiveContract [zclGWithdrawB] (BCoins 2) (ConcID "zcl-aftPutANeq")
            , RevealedSecret pb sec_b lenSecb
            , RevealedSecret pa sec_a lenSeca
        ] 
        (Time 0) 
        zclRunReva

zclRunWithdrawB :: Run
zclRunWithdrawB = 
    appendRun 
        (LWithdraw pb (BCoins 2) (ConcID "zcl-aftPutANeq")) 
        [
            ActiveContract [zclGWithdrawB] (BCoins 2) (ConcID "depo-pb-1")
            , RevealedSecret pb sec_b lenSecb
            , RevealedSecret pa sec_a lenSeca
        ] 
        (Time 0) 
        zclRunPutANeq

zclRunAfterD :: Run
zclRunAfterD = 
    appendRun 
        (LDelay timeD) 
        [
            ActiveContract zclContract (BCoins 2) (ConcID "zcl")
        ] 
        timeD 
        zclRun0


zclRunAftDWDrawA :: Run
zclRunAftDWDrawA = 
    appendRun 
        (LWithdraw pa (BCoins 2) (ConcID "zcl")) 
        [
            Deposit pa (BCoins 2) (ConcID "depo-pa-1*")
        ] 
        timeD 
        zclRunAfterD

zclRunAfterD' :: Run
zclRunAfterD' = 
    appendRun 
        (LDelay timeD') 
        [
            ActiveContract zclContract (BCoins 2) (ConcID "zcl")
        ] 
        timeD' 
        zclRun0

zclRunAftD'RevB :: Run
zclRunAftD'RevB = 
    appendRun 
        (LAuthReveal pb sec_b) 
        [
            ActiveContract zclContract (BCoins 2) (ConcID "zcl")
            , RevealedSecret pb sec_b lenSecb
        ] 
        timeD' 
        zclRunAfterD'

zclRunAftD'PutB :: Run
zclRunAftD'PutB = 
    appendRun 
        (LPutReveal [] [sec_b] (PBtwn (EInt 0) (ELength sec_b) (EInt 1)) (ConcID "zcl") zclCAftRevb) 
        [
            ActiveContract zclCAftRevb (BCoins 2) (ConcID "zcl-aftDPutB")
            , RevealedSecret pb sec_b lenSecb
        ] 
        timeD' 
        zclRunAftD'RevB

zclRunAftD'WDrawB :: Run
zclRunAftD'WDrawB = 
    appendRun 
        (LWithdraw pb (BCoins 2) (ConcID "zcl-aftDPutB")) 
        [
            Deposit pb (BCoins 2) (ConcID "depo-pb-1*")
            , RevealedSecret pb sec_b lenSecb
        ] 
        timeD' 
        zclRunAftD'PutB