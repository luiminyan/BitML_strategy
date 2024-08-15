module OperatorTest (
    resolveCIDTest
 ) where

import Test.HUnit
import Syntax.Common
import Syntax.Run
import Syntax.Label
import Syntax.Contract
import Semantic.Operators
import Semantic.Environment


testContract :: Contract
testContract = [PutReveal [] [] PTrue [
        Split [(BCoins 1, [Withdraw (Participant "PA")]), (BCoins 2, [Withdraw (Participant "PB")])]
    ]]

testContractSplit :: Contract
testContractSplit = [Split [(BCoins 1, [Withdraw (Participant "PA")]), (BCoins 2, [Withdraw (Participant "PB")])]]

contractWithdrawA :: Contract
contractWithdrawA = [Withdraw (Participant "PA")]

contractWithdrawB :: Contract
contractWithdrawB = [Withdraw (Participant "PB")]


run0 :: Run
run0 = Run ([ActiveContract testContract (BCoins 3) (ConcID "x")], [])

run1 :: Run
run1 = Run ([ActiveContract testContract (BCoins 3) (ConcID "x")], [
                (LPutReveal [] [] PTrue (ConcID "x") testContractSplit
                    , [ActiveContract testContractSplit (BCoins 3) (ConcID "conc-y")]
                    , Time 0)
            ])
            
run2 :: Run
run2 = Run ([ActiveContract testContract (BCoins 3) (ConcID "x")], [
                (LPutReveal [] [] PTrue (ConcID "x") testContractSplit
                    , [ActiveContract testContractSplit (BCoins 3) (ConcID "conc-y")]
                    , Time 0),
                (LSplit (ConcID "conc-y") [(BCoins 1, contractWithdrawA), (BCoins 1, contractWithdrawB)]
                    , [ActiveContract contractWithdrawA (BCoins 1) (ConcID "conc-y0"), ActiveContract contractWithdrawB (BCoins 2) (ConcID "conc-y1")]
                    , Time 0)
            ])

run3a :: Run
run3a = Run ([ActiveContract testContract (BCoins 3) (ConcID "x")], [
                (LPutReveal [] [] PTrue (ConcID "x") testContractSplit
                    , [ActiveContract testContractSplit (BCoins 3) (ConcID "conc-y")]
                    , Time 0),
                (LSplit (ConcID "conc-y") [(BCoins 1, contractWithdrawA), (BCoins 1, contractWithdrawB)]
                    , [ActiveContract contractWithdrawA (BCoins 1) (ConcID "conc-y0"), ActiveContract contractWithdrawB (BCoins 2) (ConcID "conc-y1")]
                    , Time 0),
                (LWithdraw (Participant "pa") (BCoins 1) (ConcID "conc-y0")
                    , [Deposit (Participant "pa") (BCoins 1) (ConcID "deposit-y0")]
                    , Time 0
                )]
            )



env1 :: Environment
env1 = setFromListEnv [(VarID "y", (LPutReveal [] [] PTrue (CID (ConcID "x")) testContractSplit, 0))]
env2 :: Environment
env2 = setFromListEnv [(VarID "y", (LPutReveal [] [] PTrue (CID (ConcID "x")) testContractSplit, 0)),
                        (VarID "y0", (LSplit (VID (VarID "y")) [(BCoins 1, contractWithdrawA), (BCoins 1, contractWithdrawB)], 0))]




resolveCIDTest :: Test
resolveCIDTest = TestList [
    TestCase (assertEqual "resolve CID x" (ConcID "x") (resolveID (CID (ConcID "x")) emptyEnv run0))
    , TestCase (assertEqual "resolve VID y" (ConcID "conc-y") (resolveID (VID (VarID "y")) env1 run1))
    , TestCase (assertEqual "resolve VID y" (ConcID "conc-y0") (resolveID (VID (VarID "y0")) env2 run2))
    ]