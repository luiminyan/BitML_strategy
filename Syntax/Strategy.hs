module Syntax.Strategy where
import Syntax.Common (Time, ID)
import Syntax.Label (Label)
import Syntax.Run (Run(..))

data Operator = Operator    -- A place holder for operator


data AbstractStrategy = 
    Do Label 
    | DoNothing
    | Wait Time
    | Combination Operator AbstractStrategy AbstractStrategy
    | IfNextThenElse Label [ID] AbstractStrategy AbstractStrategy   
    | BeforeAfter Time AbstractStrategy AbstractStrategy  


data StrategyResult = 
    Action [Label]
    | SWait Time


-- CS: functions (Run -> StrategyResult)
type ConcreteStrategy = Run -> StrategyResult



-- evaluation : see AbstractExpr -> ConcreteExpr in FP course
-- eval :: AbstractStrategy -> ConcreteStrategy
