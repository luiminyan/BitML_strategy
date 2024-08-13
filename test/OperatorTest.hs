module OperatorTest (
    resolveCIDTest
 ) where

import Test.HUnit
import Syntax.Common
import Syntax.Strategy
import Syntax.Run
import Syntax.Label
import Syntax.Contract
import Semantic.Operators
import Semantic.Environment


gc = PutReveal [] [] PTrue [
        Split [(BCoins 1, [Withdraw (Participant "PA")]), (BCoins 2, [Withdraw (Participant "PB")])]
    ]
gc_split = Split [(BCoins 1, [Withdraw (Participant "PA")]), (BCoins 2, [Withdraw (Participant "PB")])]
gc_withdraw = Withdraw (Participant "PA")
gc_withdraw_b = Withdraw (Participant "PB")


run0 = Run (([ActiveContract [gc] (BCoins 3) (ConcID "x")], InitTime), [])
run1 = Run (([ActiveContract [gc] (BCoins 3) (ConcID "x")], InitTime), [
                (LPutReveal [] [] PTrue (ConcID "x") gc_split
                    , [ActiveContract [gc_split] (BCoins 3) (ConcID "conc-y")]
                    , Time 0)
            ])
run2 = Run (([ActiveContract [gc] (BCoins 3) (ConcID "x")], InitTime), [
                (LPutReveal [] [] PTrue (ConcID "x") gc_split
                    , [ActiveContract [gc_split] (BCoins 3) (ConcID "conc-y")]
                    , Time 0),
                (LSplit (ConcID "conc-y") gc_withdraw
                    , [ActiveContract [gc_withdraw] (BCoins 1) (ConcID "conc-y0"), ActiveContract [gc_withdraw_b] (BCoins 2) (ConcID "conc-y1")]
                    , Time 0)
            ])
run3a = Run (([ActiveContract [gc] (BCoins 3) (ConcID "x")], InitTime), [
                (LPutReveal [] [] PTrue (ConcID "x") gc_split
                    , [ActiveContract [gc_split] (BCoins 3) (ConcID "conc-y")]
                    , Time 0),
                (LSplit (ConcID "conc-y") gc_withdraw
                    , [ActiveContract [gc_withdraw] (BCoins 1) (ConcID "conc-y0"), ActiveContract [gc_withdraw_b] (BCoins 2) (ConcID "conc-y1")]
                    , Time 0),
                (LWithdraw (Participant "pa") (BCoins 1) (ConcID "conc-y0")
                    , [Deposit (Participant "pa") (BCoins 1) (ConcID "deposit-y0")]
                    , Time 0
                )]
            )


env0 = emptyEnv
env1 = setFromListEnv [((VarID "y"), (LPutReveal [] [] PTrue (CID (ConcID "x")) gc_split, 0))] 
env2 = setFromListEnv [((VarID "y"), (LPutReveal [] [] PTrue (CID (ConcID "x")) gc_split, 0)),
                        ((VarID "y0"), (LSplit (VID (VarID "y")) gc_withdraw, 0))] 




resolveCIDTest :: Test
resolveCIDTest = TestList [
    TestCase (assertEqual "resolve CID" (ConcID "x") (resolveID (CID (ConcID "x")) emptyEnv run0))
    , TestCase (assertEqual "resolve VID" (ConcID "conc-y") (resolveID (VID (VarID "y")) env1 run1))
    , TestCase (assertEqual "resolve VID" (ConcID "conc-y") (resolveID (VID (VarID "y")) env1 run1))
    ]