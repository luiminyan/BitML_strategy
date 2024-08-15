module RunTest (
    runTestList
) where

import Syntax.Run
import Syntax.Label
import Syntax.Common
import Syntax.Contract
import Test.HUnit


run0 :: Run
run0 = Run ([ActiveContract [] (BCoins 1) (ConcID "tc")], [])

run1 :: Run
run1 = Run (
            [ActiveContract [] (BCoins 1) (ConcID "tc")]
            , [
                (LAuthReveal (Participant "A") (Secret "a")
                , [ActiveContract [] (BCoins 1) (ConcID "tc")
                    , RevealedSecret (Participant "A") (Secret "a") 10]
                , Time 0)
            ]
        )

run2 :: Run
run2 = Run (
            [ActiveContract [] (BCoins 1) (ConcID "tc")]
            , [
                (LAuthReveal (Participant "A") (Secret "a")
                , [ActiveContract [] (BCoins 1) (ConcID "tc")
                    , RevealedSecret (Participant "A") (Secret "a") 10]
                , Time 0)
                , (LPutReveal [] [Secret "a"] PTrue (ConcID "tc") [Withdraw (Participant "A")]
                    , [ActiveContract [] (BCoins 1) (ConcID "x")
                        , RevealedSecret (Participant "A") (Secret "a") 10]
                    , Time 0
                )
            ]
        )


transRun1 :: [(Label ConcID, ((Configuration, Time), (Configuration, Time)))]
transRun1 = [(LAuthReveal (Participant "A") (Secret "a")
                , (([ActiveContract [] (BCoins 1) (ConcID "tc")], Time 0), ([ActiveContract [] (BCoins 1) (ConcID "tc"), RevealedSecret (Participant "A") (Secret "a") 10], Time 0)))]


transRun2 :: [(Label ConcID, ((Configuration, Time), (Configuration, Time)))]
transRun2 = [(LAuthReveal (Participant "A") (Secret "a"), (([ActiveContract [] (BCoins 1) (ConcID "tc")], Time 0), ([ActiveContract [] (BCoins 1) (ConcID "tc"), RevealedSecret (Participant "A") (Secret "a") 10], Time 0)))
                , (LPutReveal [] [Secret "a"] PTrue (ConcID "tc") [Withdraw (Participant "A")], (([ActiveContract [] (BCoins 1) (ConcID "tc"), RevealedSecret (Participant "A") (Secret "a") 10], Time 0), ([ActiveContract [] (BCoins 1) (ConcID "x"), RevealedSecret (Participant "A") (Secret "a") 10], Time 0)))
            ]


transformRunTest :: Test
transformRunTest = TestList [
        TestCase (assertEqual "Transform init run" [] (transformRun run0))
        , TestCase (assertEqual "Transform run with one label" transRun1 (transformRun run1))
        , TestCase (assertEqual "Transform run with more labels" transRun2 (transformRun run2))
    ]


searchTransRunTest :: Test
searchTransRunTest = TestList [
        TestCase (assertEqual "Search a label in transformed run (exist)" (Just (([ActiveContract [] (BCoins 1) (ConcID "tc"), RevealedSecret (Participant "A") (Secret "a") 10], Time 0), ([ActiveContract [] (BCoins 1) (ConcID "x"), RevealedSecret (Participant "A") (Secret "a") 10], Time 0))) (searchTransRun (LPutReveal [] [Secret "a"] PTrue (ConcID "tc") [Withdraw (Participant "A")]) transRun2))
        , TestCase (assertEqual "Search a label in transformed run (not exist)" Nothing (searchTransRun (LDelay (Time 1)) transRun1))
        , TestCase (assertEqual "Search a label in empty trans-run" Nothing (searchTransRun (LAuthReveal (Participant "A") (Secret "a")) []))
    ]  


runTestList :: Test
runTestList = TestList [transformRunTest, searchTransRunTest]