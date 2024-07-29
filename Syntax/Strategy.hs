module Syntax.Strategy where
import Syntax.Common (Time, ID, Pred, Condition)
import Syntax.Label (Label)
import Syntax.Run (Run(..))
import NewSet (NewSet)

data Operator = Operator    -- A place holder for operator



data AbstractStrategy = 
    Do Label 
    | DoNothing
    | WaitUntil Time
    | Combination Operator AbstractStrategy AbstractStrategy
    | ExecutedThenElse Label AbstractStrategy AbstractStrategy  
    | IfThenElse Condition AbstractStrategy AbstractStrategy        -- Condition: check Time | Pred
    


-- Result of a strategy on a given run
data StrategyResult = 
    Action (NewSet Label)       -- | Action NewSet [Label]
    | Delay Time


-- CS: functions (Run -> StrategyResult)
type ConcreteStrategy = Run -> StrategyResult



-- -- evaluation : see AbstractExpr -> ConcreteExpr in FP course
-- eval :: AbstractStrategy -> ConcreteStrategy
-- eval IfThenElse p as1 as2 =
--     case p of
--         | 
        
