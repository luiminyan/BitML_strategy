module Examples.Escrow.EscrowStrategies (
    paEscrowStrategy
) where


import Syntax.Common
import Syntax.Strategy
import Examples.Escrow.EscrowContract


paEscrowStrategy :: AbstractStrategy
paEscrowStrategy = DoNothing