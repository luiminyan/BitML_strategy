module Main (main) where
import Semantic.Operators (eval)
import Semantic.Environment (emptyEnv)
import Examples.TimedCommitment.TCRuns
import Examples.TimedCommitment.TCStrategies
import Examples.ZeroCollateralLottery.ZCLRuns
import Examples.ZeroCollateralLottery.ZCLStrategies

import Examples.RockPaperScissors.RPSRuns
import Examples.RockPaperScissors.RPSStrategies


printStyle :: String -> String -> String -> IO()
printStyle pName conName runName = putStrLn $ "Execution result of " ++ pName ++ "'s strategy for contract " ++ conName ++ " on run " ++ runName ++ ":" 

main :: IO ()
main = do
    let evalPATC = eval emptyEnv paTimedCommitment
    printStyle "A" "TC" "r0"
    print $ evalPATC run_tc_0
    putStr "\n"
    printStyle "A" "TC" "rReva"
    print $ evalPATC runTCReva
    putStr "\n"
    printStyle "A" "TC" "rPuta"
    print $ evalPATC runTCPuta
    putStr "\n"
    printStyle "A" "TC" "rPuta"
    print $ evalPATC runTCDelayT
    putStr "\n"

    let evalPBTC = eval emptyEnv pbTimedCommitment
    printStyle "B" "TC" "r0"
    print $ evalPBTC run_tc_0
    putStr "\n"
    printStyle "B" "TC" "rReva"
    print $ evalPBTC runTCReva
    putStr "\n"
    printStyle "B" "TC" "rPuta"
    print $ evalPBTC runTCPuta
    putStr "\n"
    printStyle "B" "TC" "rDelayT"
    print $ evalPBTC runTCDelayT
    putStr "\n"
    

    let evalPAZCL = eval emptyEnv paZCLStrategy
    printStyle "A" "ZCL" "r0"
    print $ evalPAZCL zclRun0
    putStr "\n"
    printStyle "A" "ZCL" "rRevb"
    print $ evalPAZCL zclRunRevb
    putStr "\n"
    printStyle "A" "ZCL" "rRutb"
    print $ evalPAZCL zclRunPutb
    putStr "\n"
    printStyle "A" "ZCL" "rReva"
    print $ evalPAZCL zclRunReva
    putStr "\n"
    printStyle "A" "ZCL" "rPuta"
    print $ evalPAZCL zclRunPuta
    putStr "\n"
    printStyle "A" "ZCL" "rwDrawA"
    print $ evalPAZCL zclRunWithdrawA
    putStr "\n"
    printStyle "A" "ZCL" "aftD"
    print $ evalPAZCL zclRunAfterD
    putStr "\n"
    printStyle "A" "ZCL" "aftDWDrawA"
    print $ evalPAZCL zclRunAftDWDrawA
    putStr "\n"
    

    let evalPBZCL = eval emptyEnv pbZCLStrategy
    printStyle "B" "ZCL" "0"
    print $ evalPBZCL zclRun0
    putStr "\n"
    printStyle "B" "ZCL" "revb"
    print $ evalPBZCL zclRunRevb
    putStr "\n"
    printStyle "B" "ZCL" "putb"
    print $ evalPBZCL zclRunPutb
    putStr "\n"
    printStyle "B" "ZCL" "reva"
    print $ evalPBZCL zclRunReva
    putStr "\n"
    printStyle "B" "ZCL" "putANeq"
    print $ evalPBZCL zclRunPutANeq
    putStr "\n"
    printStyle "B" "ZCL" "wDrawB"
    print $ evalPBZCL zclRunWithdrawB
    putStr "\n"
    printStyle "B" "ZCL" "aftD'"
    print $ evalPBZCL zclRunAfterD'
    putStr "\n"
    printStyle "B" "ZCL" "aftD'RevB"
    print $ evalPBZCL zclRunAftD'RevB
    putStr "\n"
    printStyle "B" "ZCL" "aftD'PutB"
    print $ evalPBZCL zclRunAftD'PutB
    putStr "\n"
    printStyle "B" "ZCL" "aftD'wDrawB"
    print $ evalPBZCL zclRunAftD'WDrawB
    putStr "\n"


    let evalPARPS = eval emptyEnv paRPSStrategy
    printStyle "A" "RPS" "0"
    print $ evalPARPS runRPS0
    putStr "\n"
    printStyle "A" "RPS" "split"
    print $ evalPARPS runRPSSplit
    putStr "\n"
    printStyle "A" "RPS" "aftD"
    print $ evalPARPS runRPSAftD
    putStr "\n"
    printStyle "A" "RPS" "wDrawA0"
    print $ evalPARPS runRPSWDrawA0
    putStr "\n"
    printStyle "A" "RPS" "revSecA"
    print $ evalPARPS runRPSRevSecA
    putStr "\n"
    printStyle "A" "RPS" "putA"
    print $ evalPARPS runRPSPutA
    putStr "\n"
    printStyle "A" "RPS" "wDrawA1"
    print $ evalPARPS runRPSWDrawA1
    putStr "\n"
    printStyle "A" "RPS" "revB"
    print $ evalPARPS runRPSRevB
    putStr "\n"
    printStyle "A" "RPS" "putABWinA"
    print $ evalPARPS runRPSPutABWinA      -- only winA
    putStr "\n"
    printStyle "A" "RPS" "wDrawA2WinA"
    print $ evalPARPS runRPSWDrawA2WinA    -- only winA
    putStr "\n"
    printStyle "A" "RPS" "revB"
    print $ evalPARPS runRPSRevB           -- draw
    putStr "\n"
    printStyle "A" "RPS" "putABDraw"
    print $ evalPARPS runRPSPutABDraw
    putStr "\n"
    printStyle "A" "RPS" "split2"
    print $ evalPARPS runRPS2Split
    putStr "\n"
    printStyle "A" "RPS" "wDrawA3"
    print $ evalPARPS runRPS3WDrawA
    putStr "\n"
