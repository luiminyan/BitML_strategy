module NewSetTest (
    newSetTest
) where
import Test.HUnit
import NewSet

-- Test cases for elemSet
testElemSet :: Test
testElemSet = TestList [
    TestCase (assertEqual "elemSet EmptySet 1" False (elemSet EmptySet 1)),
    TestCase (assertEqual "elemSet (UnordSet [1,2,3]) 2" True (elemSet (UnordSet [1,2,3]) 2)),
    TestCase (assertEqual "elemSet (UnordSet [1,2,3]) 4" False (elemSet (UnordSet [1,2,3]) 4))
    ]

-- Test cases for insertElem
testInsertElem :: Test
testInsertElem = TestList [
    TestCase (assertEqual "insertElem EmptySet 1" (UnordSet [1]) (insertElem EmptySet 1)),
    TestCase (assertEqual "insertElem (UnordSet [1,2,3]) 4" (UnordSet [1,2,3,4]) (insertElem (UnordSet [1,2,3]) 4)),
    TestCase (assertEqual "insertElem (UnordSet [1,2,3]) 2" (UnordSet [1,2,3]) (insertElem (UnordSet [1,2,3]) 2))
    ]

-- Test cases for fromListSet
testFromListSet :: Test
testFromListSet = TestList [
    TestCase (assertEqual "fromListSet EmptySet [1,2,3]" (UnordSet [1,2,3]) (fromListSet EmptySet [1,2,3])),
    TestCase (assertEqual "fromListSet (UnordSet [1]) [2,3]" (UnordSet [1,2,3]) (fromListSet (UnordSet [1]) [2,3]))
    ]

-- Test cases for unionSet
testUnionSet :: Test
testUnionSet = TestList [
    TestCase (assertEqual "unionSet EmptySet (UnordSet [1,2,3])" (UnordSet [1,2,3]) (unionSet EmptySet (UnordSet [1,2,3]))),
    TestCase (assertEqual "unionSet (UnordSet [1,2]) (UnordSet [2,3])" (UnordSet [1,2,3]) (unionSet (UnordSet [1,2]) (UnordSet [2,3])))
    ]

-- Test cases for negationSet
testNegationSet :: Test
testNegationSet = TestList [
    TestCase (assertEqual "negationSet (UnordSet [1,2,3]) (UnordSet [2,3])" (UnordSet [1]) (negationSet (UnordSet [1,2,3]) (UnordSet [2,3]))),
    TestCase (assertEqual "negationSet (UnordSet [1,2,3]) EmptySet" (UnordSet [1,2,3]) (negationSet (UnordSet [1,2,3]) EmptySet))
    ]

-- Test cases for mapSet
testMapSet :: Test
testMapSet = TestList [
    TestCase (assertEqual "mapSet (+1) (UnordSet [1,2,3])" (UnordSet [2,3,4]) (mapSet (+1) (UnordSet [1,2,3]))),
    TestCase (assertEqual "mapSet (*2) (UnordSet [1,2,3])" (UnordSet [2,4,6]) (mapSet (*2) (UnordSet [1,2,3])))
    ]

-- Test cases for toListSet
testToListSet :: Test
testToListSet = TestList [
    TestCase (assertEqual "toListSet (+1) (UnordSet [1,2,3])" [2,3,4] (toListSet (+1) (UnordSet [1,2,3]))),
    TestCase (assertEqual "toListSet (*2) (UnordSet [1,2,3])" [2,4,6] (toListSet (*2) (UnordSet [1,2,3])))
    ]

-- Test cases for filterSet
testFilterSet :: Test
testFilterSet = TestList [
    TestCase (assertEqual "filterSet even (UnordSet [1,2,3,4])" (UnordSet [2,4]) (filterSet even (UnordSet [1,2,3,4]))),
    TestCase (assertEqual "filterSet (>2) (UnordSet [1,2,3,4])" (UnordSet [3,4]) (filterSet (>2) (UnordSet [1,2,3,4])))
    ]

-- Test cases for isEmptySet
testIsEmptySet :: Test
testIsEmptySet = TestList [
    TestCase (assertEqual "isEmptySet EmptySet" True (isEmptySet EmptySet)),
    TestCase (assertEqual "isEmptySet (UnordSet [1,2,3])" False (isEmptySet (UnordSet [1,2,3])))
    ]



newSetTest :: Test
newSetTest =  TestList [testElemSet, testInsertElem, testFromListSet, testUnionSet, testNegationSet, testMapSet, testToListSet, testFilterSet, testIsEmptySet]
