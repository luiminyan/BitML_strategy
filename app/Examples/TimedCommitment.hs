module Examples.TimedCommitment (
    paTimedCommitment
    , singleDoLabel
    , singleDoNothing
) where

import Syntax.Common (Participant (Participant), Secret (Secret), Money (BCoins), ConcID (ConcID), Time (Time), ID (..), Pred (..), VarID (..))
import Syntax.Strategy
import Syntax.Label
import Syntax.Contract (GuardedContract(Withdraw))

-- represent A's honest strategy for Timed Commitment
paTimedCommitment :: AbstractStrategy
paTimedCommitment = 
    IfThenElse (BeforeTimeOut (Time 2)) 
        (ExecutedThenElse (LAuthReveal (Participant "A") (Secret "a")) [] 
            (ExecutedThenElse (LPutReveal [] [Secret "a"] PTrue (CID (ConcID "tc")) (Withdraw (Participant "A"))) [VarID "tc'"] 
                (ExecutedThenElse (LWithdraw (Participant "A") (BCoins 1) (VID (VarID "tc'"))) []
                    DoNothing 
                    (Do (LWithdraw (Participant "A") (BCoins 1) (VID (VarID "tc'"))))
                )
                (Do (LPutReveal [] [Secret "a"] PTrue (CID (ConcID "tc")) (Withdraw (Participant "A"))))
            ) 
            (Do (LAuthReveal (Participant "A") (Secret "a")))
        ) 
        DoNothing

singleDoLabel :: AbstractStrategy
singleDoLabel = Do (LWithdraw (Participant "Tester-do-label") (BCoins 100) (CID (ConcID "x")))

singleDoNothing :: AbstractStrategy
singleDoNothing = DoNothing