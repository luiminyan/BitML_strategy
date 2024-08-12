module Examples.TimedCommitment where

import Syntax.Common (Participant (Participant), Secret (Secret), Money (BCoins), ConcID (ConcID), Time (Time), ID (..))
import Syntax.Strategy
import Syntax.Label

-- represent A's honest strategy for Timed Commitment
pa_timed_commitment = 
    IfThenElse (CheckTimeOut (Time 2)) 
        (ExecutedThenElse (LAuthReveal (Participant "A") (Secret "a")) [] 
            (ExecutedThenElse (LWithdraw (Participant "A") (BCoins 2) (CID (ConcID "tc"))) []
                DoNothing 
                (Do (LWithdraw (Participant "A") (BCoins 2) (CID (ConcID "tc"))))
            ) 
            DoNothing 
        ) 
        DoNothing

single_do_label = Do (LWithdraw (Participant "A") (BCoins 2) (CID (ConcID "tc")))
single_do_nothing = DoNothing