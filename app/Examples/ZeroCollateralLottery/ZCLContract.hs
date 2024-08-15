module Examples.ZeroCollateralLottery.ZCLContract (
    timeD
    , timeD'
    , zclContract
    , zclCAftRevb
    , zclGRevaEq
    , zclGRevaNeq
    , zclGAfterD
    , zclGWithdrawA
    , zclGWithdrawB

) where

import Syntax.Common
import Syntax.Contract

timeD :: Time
timeD = Time 3

timeD' :: Time
timeD' = Time 5     -- d' > d

zclContract :: Contract
zclContract = [PutReveal [] [Secret "b"] (PBtwn (EInt 0) (ELength (Secret "b")) (EInt 1)) zclCAftRevb, After timeD (Withdraw (Participant "PA"))]

zclCAftRevb :: Contract
zclCAftRevb = [zclGRevaEq, zclGRevaNeq, zclGAfterD]

zclGRevaEq :: GuardedContract
zclGRevaEq = PutReveal [] [Secret "a"] (PEq (ELength (Secret "a")) (ELength (Secret "b"))) [zclGWithdrawA]

zclGRevaNeq :: GuardedContract
zclGRevaNeq = PutReveal [] [Secret "a"] (PNeq (ELength (Secret "a")) (ELength (Secret "b"))) [zclGWithdrawB]

zclGAfterD :: GuardedContract
zclGAfterD = After timeD' zclGWithdrawB 

zclGWithdrawA :: GuardedContract
zclGWithdrawA = Withdraw (Participant "PA")

zclGWithdrawB :: GuardedContract
zclGWithdrawB = Withdraw (Participant "PB")