module Examples.ZeroCollateralLottery.ZCLRuns (
    zclRun0
    , zclRunRevb
    , zclRunPutb
    , zclRunReva
    , zclRunPuta
    , zclRunWithdrawA
    , zclRunAfterD
    , zclRunAftWithdrawA
) where

import Examples.ZeroCollateralLottery.ZCLContract
import Syntax.Run
import Syntax.Common
import Syntax.Label
import Syntax.Common (Secret(Secret))
import Syntax.Contract (GuardedContract(Withdraw))


-- pa :: Participant
-- pa = Participant "PA"

-- pb :: Participant
-- pb = Participant "PB"

-- sec_a :: Secret
-- sec_a = Secret "a"

-- sec_b :: Secret
-- sec_b = Secret "b"

lenSeca :: Int
lenSeca = 1

lenSecb :: Int
lenSecb = 1

zclRun0 :: Run
zclRun0 = Run ([ActiveContract zclContract (BCoins 2) (ConcID "zcl")], [])

zclRunRevb :: Run
zclRunRevb = appendRun (LAuthReveal pb sec_b) [ActiveContract zclContract (BCoins 2) (ConcID "zcl"), RevealedSecret pb sec_b lenSecb] (Time 0) zclRun0

zclRunPutb :: Run
zclRunPutb = appendRun (LPutReveal [] [sec_b] (PBtwn (EInt 0) (ELength sec_b) (EInt 1)) (ConcID "zcl") zclCAftRevb) [ActiveContract zclContract (BCoins 2) (ConcID "zcl-putb"), RevealedSecret pb sec_b lenSecb] (Time 0) zclRunRevb

zclRunReva :: Run
zclRunReva = appendRun (LAuthReveal pa sec_a) [ActiveContract zclCAftRevb (BCoins 2) (ConcID "zcl-putb"), RevealedSecret pb sec_b lenSecb, RevealedSecret pa sec_a lenSeca] (Time 0) zclRunPutb

zclRunPuta :: Run
zclRunPuta = appendRun (LPutReveal [] [sec_a] (PEq (ELength sec_a) (ELength sec_b)) (ConcID "zcl-putb") [zclGRevaEq]) [ActiveContract [zclGWithdrawA] (BCoins 2) (ConcID "zcl-puta"), RevealedSecret pb sec_b lenSecb, RevealedSecret pa sec_a lenSeca] (Time 0) zclRunReva

zclRunWithdrawA :: Run
zclRunWithdrawA = appendRun (LWithdraw pa (BCoins 2) (ConcID "zcl-puta")) [RevealedSecret pb sec_b lenSecb, RevealedSecret pa sec_a lenSeca, Deposit pa (BCoins 2) (ConcID "depo-pa-1")] (Time 0) zclRunPuta

zclRunAfterD :: Run
zclRunAfterD = appendRun (LDelay timeD) [ActiveContract zclContract (BCoins 2) (ConcID "zcl")] timeD zclRun0

zclRunAftWithdrawA :: Run
zclRunAftWithdrawA = appendRun (LWithdraw pa (BCoins 2) (ConcID "zcl")) [Deposit pa (BCoins 2) (ConcID "depo-pa-1*")] timeD zclRunAfterD