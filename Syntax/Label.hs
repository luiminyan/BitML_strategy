module Syntax.Label where

import Syntax.Common ( Secret, Money, Participant, ID, Pred )
import Syntax.Contract (GuardedContract)

data Label =
    LSplit ID GuardedContract                               -- avoid ambigious names fors successors -> in the case of choice: split(x) + split(x)
    | LAuthReveal Participant Secret
    | LPutReveal [ID] [Secret] Pred ID GuardedContract      -- avoid ambigious names fors successors -> in the case of choice: put(x) + put(x)   
    | LWithDraw Participant Money ID
    | LAuthControl Participant ID GuardedContract 
    deriving (Eq, Show)
