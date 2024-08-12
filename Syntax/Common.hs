module Syntax.Common where


newtype ConcID = ConcID String deriving (Ord, Eq, Show)     -- ConcreteID: should exist in run
newtype VarID = VarID String deriving (Ord, Eq, Show)       -- VariableID: possible ID, need to be resolved
data ID = CID ConcID | VID VarID deriving (Show)   -- active contract ID | deposit ID

instance Eq ID where
    (CID id1) == (CID id2) = id1 == id2
    (VID id1) == (VID id2) = id1 == id2 
    _ == _ = False              -- CID /= VID

instance Ord ID where
    compare (CID (ConcID id1)) (VID (VarID id2)) = GT       -- assume ConcID > VarID
    compare (VID (VarID id1)) (CID (ConcID id2)) = LT
    compare (CID id1) (CID id2) = compare id1 id2
    compare (VID id1) (VID id2) = compare id1 id2


newtype Participant = Participant String deriving (Show, Eq)  -- participants: A, B, C, D ...

newtype Money = BCoins Int deriving (Ord, Eq, Show)

newtype Secret = Secret String deriving (Ord, Eq, Show)     -- Secrect SName

data Time = Time Int | TerminationTime deriving (Show)   

instance Eq Time where
    Time t1 == Time t2 = t1 == t2
    TerminationTime == TerminationTime = True
    _ == _ = False


instance Ord Time where
    compare (Time t1) (Time t2) = compare t1 t2
    compare (Time _) TerminationTime = LT
    compare TerminationTime (Time _) = GT
    compare TerminationTime TerminationTime = EQ


-- time subtraction
subTime :: Time -> Time -> Time
subTime TerminationTime _ = error "Invalid time subtraction: termination!"
subTime (Time t1) time2 =
    if Time t1 < time2 then error "Invalid time substraction: negative time"
    else
        case time2 of
            (Time t2) -> Time $ t1 - t2



-- Logical predicates
data Pred =
    PTrue
    | PAnd Pred Pred
    | POr Pred Pred
    | PNot Pred
    | PEq E E
    | PNeq E E
    | PBtwn E E E
    | PLt E E
    deriving (Ord, Eq, Show)


-- Arithmetic expressions for logical predicates.
data E  = EInt Int
    -- Length of a secret, different from BitMLx 
    | ELength Secret
    | EAdd E E
    | ESub E E
    deriving (Ord, Eq, Show)                                


-- main = do
--     let t1 = Time 10
--     let t2 = Time 5
--     let t3 = TerminationTime
--     -- print $ subTime t1 t2       -- Time 5
--     -- print $ subTime t2 t1       -- error: negative time
--     -- print $ subTime t1 t3       -- error: termination time
--     -- print $ max t1 t2           -- Time 10
--     print $ max t1 t1              -- Time 10
--     print $ max t2 t1              -- Time 10
--     print $ max t1 t3              -- TerminationTime
    -- let id1 = VID $ VarID "a"
    -- let id2 = CID $ ConcID "a"
    -- print $ compare id1 id2