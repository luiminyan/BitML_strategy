module Syntax.Strategy where
import Syntax.Common (Time(..), ID, Pred, subTime, VarID, ConcID)
import Syntax.Label (Label)
import Syntax.Run (Run(..), InitConfiguration (..))
import NewSet 


-- data Operator = Operator    -- A place holder for operator


data Condition = CheckTimeOut Time | Predicate Pred      -- Condition will be used in strategy  


data AbstractStrategy =
    Do (Label ID)           -- ID will be evaluated (CID | VID)
    | DoNothing
    | WaitUntil Time
    | Combination AbstractStrategy AbstractStrategy         -- TODO: add Operator
    | SuccThenElse (Label ConcID) [VarID] AbstractStrategy AbstractStrategy   -- (Label ConcID) -> [VarID]
    | IfThenElse Condition AbstractStrategy AbstractStrategy        -- Condition: check Time | Pred


-- Result of a strategy on a given run
data StrategyResult =
    Actions (NewSet (Label ConcID))       -- Action NewSet [Label ConID]
    | Delay Time


newtype EvalFailure message = EvalFail message deriving (Show)


-- CS: functions (Run -> EvalFail | StrategyResult)
type ConcreteStrategy = Run -> Either (EvalFailure String) StrategyResult 





