module Examples.RockPaperScissors.RPSContract (
    pa
    , pb
    , secA
    , secB
    , timeD
    , win
    , rpsContract
    , rpsCon0
    , rpsCon1
    , rpsCon2
    , rpsCon2Split
    , gConWithdrawPA
    , gConWithdrawPB
) where

import Syntax.Common
import Syntax.Contract

pa :: Participant
pa = Participant "PA"

pb :: Participant
pb = Participant "PB"

secA :: Secret
secA = Secret "a"

secB :: Secret
secB = Secret "b"

timeD :: Time
timeD = Time 5      -- Time d in BitML paper


{- winner function from BitML paper: (lenN, lenM) = (0, 1) or (2, 1) or (1, 0)-}
win :: Secret -> Secret -> Pred 
win secretN secretM = POr (POr (PAnd (PEq (ELength secretN) (EInt 0)) (PEq (ELength secretM) (EInt 2)))
                                (PAnd (PEq (ELength secretN) (EInt 2)) (PEq (ELength secretM) (EInt 1))))
                            (PAnd (PEq (ELength secretN) (EInt 1)) (PEq (ELength secretM) (EInt 0)))

rpsContract :: Contract
rpsContract = [Split [(BCoins 2, rpsCon0), (BCoins 2, rpsCon1), (BCoins 2, rpsCon2)]]
                                                                    
rpsCon0 :: Contract
rpsCon0 = [PutReveal [] [secB] (PBtwn (EInt 0) (ELength secB) (EInt 2)) [Withdraw pb], After timeD (Withdraw pa)]

rpsCon1 :: Contract
rpsCon1 = [PutReveal [] [secA] (PBtwn (EInt 0) (ELength secA) (EInt 2)) [Withdraw pa], After timeD (Withdraw pb)]

rpsCon2 :: Contract
rpsCon2 = [PutReveal [] [secA, secB] (win secA secB) [Withdraw pa], PutReveal [] [secA, secB] (win secB secA) [Withdraw pb], PutReveal [] [secA, secB] (PEq (ELength secA) (ELength secB)) [Split [(BCoins 1, [Withdraw pa]), (BCoins 1, [Withdraw pb])]]]

rpsCon2Split :: Contract
rpsCon2Split = [Split [(BCoins 1, [Withdraw pa]), (BCoins 1, [Withdraw pb])]]

gConWithdrawPA :: GuardedContract
gConWithdrawPA = Withdraw pa

gConWithdrawPB :: GuardedContract
gConWithdrawPB = Withdraw pb

