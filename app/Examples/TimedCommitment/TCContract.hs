module Examples.TimedCommitment.TCContract (
    contractTimedCommitment
) where

import Syntax.Common
import Syntax.Contract
import Syntax.Label


contractTimedCommitment :: [GuardedContract]
contractTimedCommitment = [
        PutReveal [] [Secret "a"] PTrue [Withdraw (Participant "A")]
        , After (Time 2) (Withdraw (Participant "B"))
    ]