module Examples.TimedCommitment.TCContract (
    contractTimedCommitment
    , timeT
    , pa
    , pb
    , secA
    , secB
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

timeT :: Time
timeT = Time 2

contractTimedCommitment :: [GuardedContract]
contractTimedCommitment = [
        PutReveal [] [Secret "a"] PTrue [Withdraw (Participant "A")]
        , After (Time 2) (Withdraw (Participant "B"))
    ]
