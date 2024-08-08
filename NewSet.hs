{-|
Module      : NewSet
Description : A customized set, which includes no duplicate      
-}

module NewSet (
    NewSet (..)
    , elemSet
    , insertElem
    , insertList
    , unionSet
    , negationSet
    , mapSet
    , mapSetList
    , filterSet
    , isEmptySet
) where

import Syntax.Label (Label(LWithdraw, LSplit, LPutReveal))
import Syntax.Common (Participant(Participant), ID (..), Pred (PTrue))

data NewSet a = EmptySet | UnordSet [a] deriving (Eq, Show)


-- check if a elem is in set
elemSet :: Eq a => NewSet a -> a -> Bool
elemSet EmptySet a = False
elemSet (UnordSet as) a = a `elem` as


-- insert a new element into a set
insertElem :: Eq a => NewSet a -> a -> NewSet a
insertElem EmptySet a = UnordSet [a]
insertElem (UnordSet as) a
    | elemSet (UnordSet as) a = UnordSet as     -- if a in set, do not insert 
    | otherwise = UnordSet (as ++ [a])             -- insert elem 


-- insert a list into a set
insertList :: (Eq a) => NewSet a -> [a] -> NewSet a
insertList= foldl insertElem


-- union function (all elements from both sets)
unionSet :: Eq a => NewSet a -> NewSet a -> NewSet a
unionSet EmptySet s2 = s2
unionSet s1 EmptySet = s1
unionSet s1 (UnordSet as2) = foldl insertElem s1 as2   -- insert elem from new set to s1


-- negation: s1 - s2
negationSet EmptySet _ = EmptySet
negationSet s1 EmptySet = s1
negationSet (UnordSet s1) s2 = foldl (\acc x -> if elemSet s2 x then acc else insertElem acc x) EmptySet s1


-- map function for NewSet
mapSet :: (Eq a, Eq b) => (a -> b) -> NewSet a -> NewSet b
mapSet _ EmptySet = EmptySet
mapSet f (UnordSet s) = insertList EmptySet (map f s)


-- map NewSet into list
mapSetList :: (a -> b) -> NewSet a -> [b]
mapSetList _ EmptySet = []
mapSetList f (UnordSet l) = map f l


-- filter function on NewSet
filterSet :: Eq a => (a -> Bool) -> NewSet a -> NewSet a
filterSet _ EmptySet = EmptySet
filterSet f (UnordSet s) = foldl (\acc x -> if f x then insertElem acc x else acc) EmptySet s


-- if a set is empty
isEmptySet :: NewSet a -> Bool
isEmptySet EmptySet = True
isEmptySet _        = False

main = do
    let s1 = insertElem (UnordSet [1]) 2                         -- UnordSet [1, 2]
    let s2 = insertElem (insertElem EmptySet "Apple") "Banana"       -- UnordSet ["Apple", "Banana"]
    let s3 = insertList EmptySet [2, 4, 1, 3]

    print $ unionSet s1 s3          -- UnordSet [1, 2, 4, 3]
    print $ negationSet s3 s1       -- UnordSet [4, 3]
    print $ filterSet even s3       -- UnordSet [2, 4]   
    print $ filterSet (==5) s3      -- EmptySet
    print $ mapSet (*2) s1
    print $ mapSetList (*2) s1
    print $ filterSet (==1) s3
    print $ filterSet (==0) s3