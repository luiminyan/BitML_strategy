module Syntax.Label where

import Syntax.Common ( Secret, Money, Participant, ID, Pred )
import Syntax.Contract (GuardedContract)

data Label =
    LSplit ID
    | LAuthReveal Participant Secret
    | LPutReveal [ID] [Secret] Pred ID        
    | LWithDraw Participant Money ID
    | LAuthControl Participant ID GuardedContract 
    deriving (Eq, Show)
