import Test.HUnit
import OperatorTest
import RunTest

main :: IO Counts
main = do 
    runTestTT $ TestList [resolveCIDTest, runTestList]
