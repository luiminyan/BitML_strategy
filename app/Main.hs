module Main where

main:: IO ()
main = do
--     let secA = Secret "a"
--     let secB = Secret "b"

--     let ac_1 = ActiveContract [PutReveal [] [secB] (PBtwn (EInt 0) (getSLen secB) (EInt 2)) [Withdraw (Participant "B")]
--             , After (Time 5) (Withdraw (Participant "A"))] (BCoins 2) (ID "x1")

--     let ac_2 = ActiveContract [PutReveal [] [secA] (PBtwn (EInt 0) (getSLen secA) (EInt 2)) [Withdraw (Participant "A")]
--             , After (Time 5) (Withdraw (Participant "B"))] (BCoins 2) (ID "x2")

--     let ac_3 = ActiveContract [PutReveal [] [Secret "a", Secret "b"] (PLt (getSLen secA) (getSLen secB)) [Withdraw (Participant "A")]
--             , PutReveal [] [Secret "a", Secret "b"] (PLt (getSLen secB) (getSLen secA)) [Withdraw (Participant "B")]
--             , PutReveal [] [Secret "a", Secret "b"] (PEq (getSLen secB) (getSLen secA)) [Split [(BCoins 1, [Withdraw (Participant "A")]), (BCoins 1, [Withdraw (Participant "B")])]]] (BCoins 2) (ID "x3") 

--     let run0 = Run (InitConfig, [])
--     let run1 = Run (InitConfig, [(LSplit (ID "x"), [ac_1, ac_2, ac_3], Time 0)])    -- after split x
    
--     let time_t = Time 5         -- assume time t = 5


   

    print "Main: test"