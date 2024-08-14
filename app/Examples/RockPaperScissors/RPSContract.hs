module Examples.RockPaperScissors.RPSContract (
    timeD
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
rpsCon0 = [PutReveal [] [Secret "b"] (PBtwn (EInt 0) (ELength (Secret "b")) (EInt 2)) [Withdraw (Participant "PB")], After timeD (Withdraw (Participant "PA"))]

rpsCon1 :: Contract
rpsCon1 = [PutReveal [] [Secret "a"] (PBtwn (EInt 0) (ELength (Secret "a")) (EInt 2)) [Withdraw (Participant "PA")], After timeD (Withdraw (Participant "PB"))]

rpsCon2 :: Contract
rpsCon2 = [PutReveal [] [Secret "a", Secret "b"] (win (Secret "a") (Secret "b")) [Withdraw (Participant "PA")], PutReveal [] [Secret "a", Secret "b"] (win (Secret "b") (Secret "a")) [Withdraw (Participant "PB")], PutReveal [] [Secret "a", Secret "b"] (PEq (ELength (Secret "a")) (ELength (Secret "b"))) [Split [(BCoins 1, [Withdraw (Participant "PA")]), (BCoins 1, [Withdraw (Participant "PB")])]]]

rpsCon2Split :: Contract
rpsCon2Split = [Split [(BCoins 1, [Withdraw (Participant "PA")]), (BCoins 1, [Withdraw (Participant "PB")])]]

gConWithdrawPA :: GuardedContract
gConWithdrawPA = Withdraw (Participant "PA")

gConWithdrawPB :: GuardedContract
gConWithdrawPB = Withdraw (Participant "PB")

