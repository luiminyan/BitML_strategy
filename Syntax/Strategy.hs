module Syntax.Strategy where
import Syntax.Common (Time(..), ID, Pred, Condition, subTime)
import Syntax.Label (Label)
import Syntax.Run (Run(..), InitConfiguration (..))
import NewSet (NewSet (UnordSet))


data Operator = Operator    -- A place holder for operator


data AbstractStrategy =
    Do Label
    | DoNothing
    | WaitUntil Time
    | Combination AbstractStrategy AbstractStrategy         -- TODO: add Operator
    | ExecutedThenElse Label AbstractStrategy AbstractStrategy
    | IfThenElse Condition AbstractStrategy AbstractStrategy        -- Condition: check Time | Pred


-- Result of a strategy on a given run
data StrategyResult =
    Actions (NewSet Label)       -- Action NewSet [Label]
    | Delay Time


-- CS: functions (Run -> StrategyResult)
type ConcreteStrategy = Run -> StrategyResult



