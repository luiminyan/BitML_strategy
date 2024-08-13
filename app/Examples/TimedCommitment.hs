module Examples.TimedCommitment (
    pa_timed_commitment
    , single_do_label
    , single_do_nothing
) where

import Syntax.Common (Participant (Participant), Secret (Secret), Money (BCoins), ConcID (ConcID), Time (Time), ID (..), Pred (..), VarID (..))
import Syntax.Strategy
import Syntax.Label
import Syntax.Contract (GuardedContract(Withdraw))

-- represent A's honest strategy for Timed Commitment
pa_timed_commitment = 
    IfThenElse (BeforeTimeOut (Time 2)) 
        (ExecutedThenElse (LAuthReveal (Participant "A") (Secret "a")) [] 
            (ExecutedThenElse (LPutReveal [] [Secret "a"] PTrue (CID (ConcID "tc")) (Withdraw (Participant "A"))) [VarID "tc'"] 
                (ExecutedThenElse (LWithdraw (Participant "A") (BCoins 1) (CID (ConcID "tc"))) []
                    DoNothing 
                    (Do (LWithdraw (Participant "A") (BCoins 1) (CID (ConcID "tc"))))
                )
                (Do (LPutReveal [] [Secret "a"] PTrue (CID (ConcID "tc")) (Withdraw (Participant "A"))))
            ) 
            (Do (LAuthReveal (Participant "A") (Secret "a")))
        ) 
        DoNothing

single_do_label = Do (LWithdraw (Participant "A") (BCoins 1) (CID (ConcID "tc")))
single_do_nothing = DoNothing