module Examples.ZeroCollateralLottery.ZCLContract (
    pa
    , pb
    , sec_a
    , sec_b
    , timeD
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

pa :: Participant
pa = Participant "PA"

pb :: Participant
pb = Participant "PB"

sec_a :: Secret
sec_a = Secret "a"

sec_b :: Secret
sec_b = Secret "b"

timeD :: Time
timeD = Time 3

timeD' :: Time
timeD' = Time 5     -- d' > d

zclContract :: Contract
zclContract = [PutReveal [] [sec_b] (PBtwn (EInt 0) (ELength sec_b) (EInt 1)) zclCAftRevb, After timeD (Withdraw pa)]

zclCAftRevb :: Contract
zclCAftRevb = [zclGRevaEq, zclGRevaNeq, zclGAfterD]

zclGRevaEq :: GuardedContract
zclGRevaEq = PutReveal [] [sec_a] (PEq (ELength sec_a) (ELength sec_b)) [zclGWithdrawA]

zclGRevaNeq :: GuardedContract
zclGRevaNeq = PutReveal [] [sec_a] (PNeq (ELength sec_a) (ELength sec_b)) [zclGWithdrawB]

zclGAfterD :: GuardedContract
zclGAfterD = After timeD' zclGWithdrawB 

zclGWithdrawA :: GuardedContract
zclGWithdrawA = Withdraw pa

zclGWithdrawB :: GuardedContract
zclGWithdrawB = Withdraw pb