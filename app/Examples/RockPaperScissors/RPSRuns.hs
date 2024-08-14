module Examples.RockPaperScissors.RPSRuns (

) where

import Syntax.Common
import Syntax.Run
import Syntax.Label
import Examples.RockPaperScissors.RPSContract



runRps0 :: Run
runRps0 = Run (([ActiveContract rpsContract (BCoins 6) (ConcID "rps")], InitTime), [])

runRpsSplit :: Run
runRpsSplit = appendRun (LSplit (ConcID "rps") [(BCoins 2, rpsCon0), (BCoins 2, rpsCon1), (BCoins 2, rpsCon2)]) [ActiveContract rpsCon0 (BCoins 2) (ConcID "conc-x0"), ActiveContract rpsCon1 (BCoins 2) (ConcID "conc-x1"), ActiveContract rpsCon2 (BCoins 2) (ConcID "conc-x2")] (Time 0) runRps0

