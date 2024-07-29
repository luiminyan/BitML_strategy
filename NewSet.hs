{-|
Module      : NewSet
Description : A customized set, which includes no duplicate      
-}

module NewSet where

data NewSet a = EmptySet | UnordSet [a] deriving (Show)


-- check if a elem is in set
elemSet :: Eq a => NewSet a -> a -> Bool
elemSet EmptySet a = False
elemSet (UnordSet as) a = a `elem` as


-- insert a new element, exclude duplication
insert EmptySet a = UnordSet [a]
insert (UnordSet as) a
    | elemSet (UnordSet as) a = UnordSet as     -- if a in set, do not insert 
    | otherwise = UnordSet (as ++ [a])             -- insert elem 


-- union function
union EmptySet s2 = s2
union s1 EmptySet = s1
union s1 (UnordSet as2) = foldl insert s1 as2   -- insert elem from s2 to s1


-- main = do
--     let s1 = insert (UnordSet [1]) 2                         -- UnordSet [1, 2]
--     let s2 = insert (insert EmptySet "Apple") "Banana"       -- UnordSet ["Apple", "Banana"]
--     print s1