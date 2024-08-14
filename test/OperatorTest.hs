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


gc :: GuardedContract
gc = PutReveal [] [] PTrue [
        Split [(BCoins 1, [Withdraw (Participant "PA")]), (BCoins 2, [Withdraw (Participant "PB")])]
    ]
gcSplit :: GuardedContract
gcSplit = Split [(BCoins 1, [Withdraw (Participant "PA")]), (BCoins 2, [Withdraw (Participant "PB")])]
gcWithdraw :: GuardedContract
gcWithdraw = Withdraw (Participant "PA")
gcWithdrawB :: GuardedContract
gcWithdrawB = Withdraw (Participant "PB")


run0 :: Run
run0 = Run (([ActiveContract [gc] (BCoins 3) (ConcID "x")], InitTime), [])

run1 :: Run
run1 = Run (([ActiveContract [gc] (BCoins 3) (ConcID "x")], InitTime), [
                (LPutReveal [] [] PTrue (ConcID "x") gcSplit
                    , [ActiveContract [gcSplit] (BCoins 3) (ConcID "conc-y")]
                    , Time 0)
            ])
            
run2 :: Run
run2 = Run (([ActiveContract [gc] (BCoins 3) (ConcID "x")], InitTime), [
                (LPutReveal [] [] PTrue (ConcID "x") gcSplit
                    , [ActiveContract [gcSplit] (BCoins 3) (ConcID "conc-y")]
                    , Time 0),
                (LSplit (ConcID "conc-y") gcWithdraw
                    , [ActiveContract [gcWithdraw] (BCoins 1) (ConcID "conc-y0"), ActiveContract [gcWithdrawB] (BCoins 2) (ConcID "conc-y1")]
                    , Time 0)
            ])

run3a :: Run
run3a = Run (([ActiveContract [gc] (BCoins 3) (ConcID "x")], InitTime), [
                (LPutReveal [] [] PTrue (ConcID "x") gcSplit
                    , [ActiveContract [gcSplit] (BCoins 3) (ConcID "conc-y")]
                    , Time 0),
                (LSplit (ConcID "conc-y") gcWithdraw
                    , [ActiveContract [gcWithdraw] (BCoins 1) (ConcID "conc-y0"), ActiveContract [gcWithdrawB] (BCoins 2) (ConcID "conc-y1")]
                    , Time 0),
                (LWithdraw (Participant "pa") (BCoins 1) (ConcID "conc-y0")
                    , [Deposit (Participant "pa") (BCoins 1) (ConcID "deposit-y0")]
                    , Time 0
                )]
            )



env1 :: Environment
env1 = setFromListEnv [(VarID "y", (LPutReveal [] [] PTrue (CID (ConcID "x")) gcSplit, 0))]
env2 :: Environment
env2 = setFromListEnv [(VarID "y", (LPutReveal [] [] PTrue (CID (ConcID "x")) gcSplit, 0)),
                        (VarID "y0", (LSplit (VID (VarID "y")) gcWithdraw, 0))]




resolveCIDTest :: Test
resolveCIDTest = TestList [
    TestCase (assertEqual "resolve CID x" (ConcID "x") (resolveID (CID (ConcID "x")) emptyEnv run0))
    , TestCase (assertEqual "resolve VID y" (ConcID "conc-y") (resolveID (VID (VarID "y")) env1 run1))
    , TestCase (assertEqual "resolve VID y" (ConcID "conc-y0") (resolveID (VID (VarID "y0")) env2 run2))
    ]