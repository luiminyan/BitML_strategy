module PredicateTests (
    -- testGetSecLen
    -- , testEvalArithExpr
    -- , testEvalPred
    predicateTests
) where

import Test.HUnit
import Syntax.Common 
import Syntax.Run
import Syntax.Label    
import Semantic.Operators (evalPred, evalArithExpr, getSecLen)
import qualified Data.Map as Map

-- Sample Secrets and Runs for testing
secretA :: Secret 
secretA = Secret "a"

secretB :: Secret
secretB = Secret "b"

-- Run with secrets revealed at different times
runWithSecrets :: Run
runWithSecrets = Run (([], InitTime), 
    [ (LAuthReveal (Participant "A") secretA, [RevealedSecret (Participant "A") secretA 5], Time 1)
    , (LAuthReveal (Participant "B") secretB, [RevealedSecret (Participant "B") secretB 10], Time 2)
    ])

-- Test cases for getSecLen
testGetSecLen :: Test
testGetSecLen = TestList [
    TestCase (assertEqual "Secret A length" (Just 5) (getSecLen secretA runWithSecrets)),
    TestCase (assertEqual "Secret B length" (Just 10) (getSecLen secretB runWithSecrets)),
    TestCase (assertEqual "Unknown Secret length" Nothing (getSecLen (Secret "c") runWithSecrets))
    ]

-- Test cases for evalArithExpr
testEvalArithExpr :: Test
testEvalArithExpr = TestList [
    TestCase (assertEqual "Evaluate EInt" (Just 3) (evalArithExpr (EInt 3) runWithSecrets)),
    TestCase (assertEqual "Evaluate ELength" (Just 5) (evalArithExpr (ELength secretA) runWithSecrets)),
    TestCase (assertEqual "Evaluate EAdd" (Just 15) (evalArithExpr (EAdd (ELength secretA) (ELength secretB)) runWithSecrets)),
    TestCase (assertEqual "Evaluate ESub" (Just (-5)) (evalArithExpr (ESub (ELength secretA) (ELength secretB)) runWithSecrets))
    ]

-- Test cases for evalPred
testEvalPred :: Test
testEvalPred = TestList [
    TestCase (assertBool "Evaluate PTrue" (evalPred PTrue runWithSecrets)),
    TestCase (assertBool "Evaluate PEq (true)" (evalPred (PEq (ELength secretA) (EInt 5)) runWithSecrets)),
    TestCase (assertBool "Evaluate PEq (false)" (not (evalPred (PEq (ELength secretA) (EInt 10)) runWithSecrets))),
    TestCase (assertBool "Evaluate PAnd" (evalPred (PAnd PTrue (PEq (ELength secretA) (EInt 5))) runWithSecrets)),
    TestCase (assertBool "Evaluate POr (true)" (evalPred (POr PTrue (PEq (ELength secretA) (EInt 10))) runWithSecrets)),
    TestCase (assertBool "Evaluate POr (false)" (not (evalPred (POr (PAnd PTrue (PEq (ELength secretA) (EInt 10))) (PNot PTrue)) runWithSecrets)))
    ]

predicateTests :: Test
predicateTests = TestList [TestLabel "pred-test01" testGetSecLen, TestLabel "pred-test02" testEvalArithExpr, TestLabel "pred-test03" testEvalPred]