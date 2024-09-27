import Test.HUnit
import PredicateTests
import OperatorTest
import RunTest

main :: IO Counts
main = do 
    runTestTT $ TestList [predicateTests, resolveCIDTest, runTestList]
