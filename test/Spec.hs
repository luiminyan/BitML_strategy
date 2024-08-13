import Test.HUnit
import PredicateTests
import NewSetTest
import OperatorTest

main :: IO Counts
-- main = runTestTT $ TestList [predicateTests, newSetTest]
main = runTestTT $ TestList [resolveCIDTest]