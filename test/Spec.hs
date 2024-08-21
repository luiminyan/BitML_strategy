import Test.HUnit
import PredicateTests
import NewSetTest
import OperatorTest
import RunTest

main :: IO Counts
main = do 
    runTestTT $ TestList [predicateTests, newSetTest, resolveCIDTest, runTestList]
