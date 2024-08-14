module Examples.Escrow.EscrowContract (
    contractEscrow
) where

import Syntax.Contract
import Syntax.Common


contractPayOrRefund :: [GuardedContract]
contractPayOrRefund = [Auth [Participant "PA"] (Withdraw (Participant "PB"))
                        , Auth [Participant "PB"] (Withdraw (Participant "PA"))
                        ]

resolveEscrow :: Int -> Int -> [GuardedContract]
resolveEscrow v v' = [
        Split [(BCoins v, [Withdraw (Participant "PM")])
                , (BCoins v', [Auth [Participant "PM"] (Withdraw (Participant "PA"))
                                , Auth [Participant "PM"] (Withdraw (Participant "PB"))])
                ]
        ]


contractEscrow :: [GuardedContract]
contractEscrow = contractPayOrRefund ++ resolveEscrow 1 9 ++ resolveEscrow 9 1

