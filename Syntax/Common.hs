module Syntax.Common where


newtype ID = ID String deriving (Eq, Show)      -- Active contract / Deposite ID

newtype Participant = Participant String deriving (Show, Eq)  -- participants: A, B, C, D ...

newtype Money = BCoins Int deriving (Eq, Show)

newtype Secret = Secret String deriving (Eq, Show)     -- Secrect SName

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
subTime TerminationTime _ = error "Invalid subtraction: infinite!"
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
    deriving (Eq, Ord, Show)


-- Arithmetic expressions for logical predicates.
data E  = EInt Integer
    -- Length of a secret.
    | ELength String
    | EAdd E E
    | ESub E E
    deriving (Eq, Ord, Show)


data Condition = Check Time | Pred      -- Condition will be used in strategy                                    


-- -- Get the length of a secrect
-- getSLen :: Secret -> E
-- getSLen (Secret sName) = ELength sName


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
--     print "Common.hs"