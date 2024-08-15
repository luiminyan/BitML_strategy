module Examples.ZeroCollateralLottery.ZCLRuns (
    zclRun0
) where

import Examples.ZeroCollateralLottery.ZCLContract
import Syntax.Run
import Syntax.Common
import Syntax.Label

zclRun0 :: Run
zclRun0 = Run (([ActiveContract zclContract (BCoins 2) (ConcID "zcl")], InitTime), [])

