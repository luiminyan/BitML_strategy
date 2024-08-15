module Examples.Escrow.EscrowRuns (
        runEscrow0
) where

import Syntax.Common
import Syntax.Label
import Syntax.Run
import Examples.Escrow.EscrowContract 




runEscrow0 :: Run 
runEscrow0 = Run ([ActiveContract contractEscrow (BCoins 10) (ConcID "escrow")], [])

-- runEscrow

-- paEscrowStrategy :: AbstractStrategy
-- paEscrowStrategy = ExecutedThenElse () [] () ()

