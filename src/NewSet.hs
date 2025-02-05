{-|
Module      : NewSet
Description : A customized set, which includes no duplicate      
-}

module NewSet (
    NewSet (..)
    , elemSet
    , insertElem
    , fromListSet
    , unionSet
    , negationSet
    , mapSet
    , toListSet
    , filterSet
    , isEmptySet
) where



data NewSet a = EmptySet | UnordSet [a] deriving (Eq, Show)


-- check if a elem is in set
elemSet :: Eq a => NewSet a -> a -> Bool
elemSet EmptySet _ = False
elemSet (UnordSet as) a = a `elem` as


-- insert a new element into a set
insertElem :: Eq a => NewSet a -> a -> NewSet a
insertElem EmptySet a = UnordSet [a]
insertElem (UnordSet as) a
    | elemSet (UnordSet as) a = UnordSet as     -- if a in set, do not insert 
    | otherwise = UnordSet (as ++ [a])             -- insert elem 


-- insert a list into a set
fromListSet :: (Eq a) => NewSet a -> [a] -> NewSet a
fromListSet= foldl insertElem


-- union function (all elements from both sets)
unionSet :: Eq a => NewSet a -> NewSet a -> NewSet a
unionSet EmptySet s2 = s2
unionSet s1 EmptySet = s1
unionSet s1 (UnordSet as2) = foldl insertElem s1 as2   -- insert elem from new set to s1


-- negation: s1 - s2
negationSet :: Eq a => NewSet a -> NewSet a -> NewSet a
negationSet EmptySet _ = EmptySet
negationSet s1 EmptySet = s1
negationSet (UnordSet s1) s2 = foldl (\acc x -> if elemSet s2 x then acc else insertElem acc x) EmptySet s1


-- map function for NewSet
mapSet :: (Eq b) => (a -> b) -> NewSet a -> NewSet b
mapSet _ EmptySet = EmptySet
mapSet f (UnordSet s) = fromListSet EmptySet (map f s)


-- map NewSet into list
toListSet :: (a -> b) -> NewSet a -> [b]
toListSet _ EmptySet = []
toListSet f (UnordSet l) = map f l


-- filter function on NewSet
filterSet :: Eq a => (a -> Bool) -> NewSet a -> NewSet a
filterSet _ EmptySet = EmptySet
filterSet f (UnordSet s) = foldl (\acc x -> if f x then insertElem acc x else acc) EmptySet s


-- if a set is empty
isEmptySet :: NewSet a -> Bool
isEmptySet EmptySet = True
isEmptySet _        = False
