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
import Syntax.Contract (GuardedContract(PutReveal))



secretA :: Secret 
secretA = Secret "a"

secretB :: Secret
secretB = Secret "b"

-- Run without secret
runNoSecret :: Run
runNoSecret = Run (([], InitTime), [(LDelay (Time 1), [ActiveContract [PutReveal [] [secretA, secretB] PTrue []] (BCoins 1) (ConcID "x")], Time 1)])

-- Run with secrets a and b
runWithSecrets :: Run
runWithSecrets = Run (([], InitTime), 
    [ (LAuthReveal (Participant "A") secretA, [RevealedSecret (Participant "A") secretA 5], Time 0)
    , (LAuthReveal (Participant "B") secretB, [RevealedSecret (Participant "A") secretA 5, RevealedSecret (Participant "B") secretB 10], Time 0)
    ])

-- Test getSecLen
testGetSecLen :: Test
testGetSecLen = TestList [
    TestCase (assertEqual "Secret A length" (Just 5) (getSecLen secretA runWithSecrets)),
    TestCase (assertEqual "Secret B length in runNoSec" Nothing (getSecLen secretB runNoSecret)),
    TestCase (assertEqual "Unknown Secret length" Nothing (getSecLen (Secret "c") runWithSecrets))
    ]

-- Test evalArithExpr
testEvalArithExpr :: Test
testEvalArithExpr = TestList [
    TestCase (assertEqual "Evaluate EInt" (Just 3) (evalArithExpr (EInt 3) runWithSecrets)),
    TestCase (assertEqual "Evaluate ELength (exist)" (Just 5) (evalArithExpr (ELength secretA) runWithSecrets))
    , TestCase (assertEqual "Evaluate ELength (not exist)" Nothing (evalArithExpr (ELength secretA) runNoSecret))
    , TestCase (assertEqual "Evaluate EAdd" (Just 15) (evalArithExpr (EAdd (ELength secretA) (EInt 10)) runWithSecrets)),
    TestCase (assertEqual "Evaluate ESub" (Just (-5)) (evalArithExpr (ESub (ELength secretA) (ELength secretB)) runWithSecrets))
    ]

-- Test evalPred
testEvalPred :: Test
testEvalPred = TestList [
    TestCase (assertBool "Evaluate PTrue: true = true" (evalPred PTrue runWithSecrets))
    , TestCase (assertBool "Evaluate PEq (true): (len_a == 5) = true" (evalPred (PEq (ELength secretA) (EInt 5)) runWithSecrets))
    , TestCase (assertBool "Evaluate PEq (false): (len_a == len_b) = false" (not (evalPred (PEq (ELength secretA) (ELength secretB)) runWithSecrets)))
    , TestCase (assertBool "Evaluate PAnd: true and (1 == 1) = true" (evalPred (PAnd PTrue (PEq (EInt 1) (EInt 1))) runWithSecrets))
    , TestCase (assertBool "Evaluate POr: true or (1 == 0) = true" (evalPred (POr PTrue (PEq (EInt 1) (EInt 0))) runWithSecrets))
    , TestCase (assertBool "Evaluate PNot: not (1 == 0) = true" (evalPred (PNot (PEq (EInt 1) (EInt 0))) runWithSecrets))
    ]

predicateTests :: Test
predicateTests = TestList [TestLabel "pred-test01" testGetSecLen, TestLabel "pred-test02" testEvalArithExpr, TestLabel "pred-test03" testEvalPred]