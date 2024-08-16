module Syntax.Strategy (
    Condition (..)
    , AbstractStrategy (..)
    , StrategyResult (..)
    , ConcreteStrategy
) where
    
import Syntax.Common (Time(..), ID(..), VarID, ConcID, Pred)
import Syntax.Label (Label)
import Syntax.Run (Run(..))
import NewSet 


-- data Operator = Operator    -- A place holder for operator


data Condition = BeforeTimeOut Time | Predicate Pred deriving (Show)      -- Condition will be used in strategy  


data AbstractStrategy =
    Do (Label ID)           -- ID will be evaluated (CID | VID)
    | DoNothing
    | WaitUntil Time
    | Combination AbstractStrategy AbstractStrategy         -- TODO: add Operator
    | ExecutedThenElse (Label ID) [VarID] AbstractStrategy AbstractStrategy   -- (Label CID|VID) -> [VarID]
    | IfThenElse Condition AbstractStrategy AbstractStrategy        -- Condition: check Time | Pred
    deriving (Show)

-- Result of a strategy on a given run
data StrategyResult =
    Actions (NewSet (Label ConcID))       -- Action NewSet [Label ConID]
    | Delay Time
    deriving (Show)

-- newtype EvalFailure message = EvalFail message deriving (Show)


-- TODO: CS: functions (Run -> EvalFail | StrategyResult)
type ConcreteStrategy = Run -> StrategyResult 






