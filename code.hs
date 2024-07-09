import Data.List
import Text.Printf (errorBadArgument)

-- Type definition

newtype ID = ID String deriving (Eq, Show)      -- Active contract / Deposite ID

newtype Participant = P String deriving (Show, Eq)  -- participants: A, B, C, D ...

newtype Money = BCoins Int deriving (Eq, Show)

data Secret = Secret String Int deriving (Eq, Show)     -- Secrect SName SLength

newtype Time = Time Int     -- timeout


-- Labels
data Label =
    Split ID
    | AuthReveal Participant Secret
    | PutReveal [ID] [Secret] ID        -- put [DepositID] [Secrect] ID
    | WithDraw Participant Money ID
    | AuthControl Participant ID
    | Control ID
    deriving (Eq, Show)


-- Configurations objects
data ConfigObject =
    ActiveContract Money ID
    | SecretCommit Participant Secret
    | RevealedSecrect  Participant Secret
    | AuthTakeBranch Participant ID
    | Deposite Participant Money ID
    -- Time?
    deriving (Show)


type Configuration = [ConfigObject]


newtype Run = Run [(Label, Configuration)] deriving (Show)


-- strategy :: Run -> Either [Label] Time



-- greedyCombination
-- choices: 1. Union 2. Intersect  3. cvUnionComb 

unionComb :: [Label] -> [Label] -> [Label]
unionComb = union

intersectComb :: [Label] -> [Label] -> [Label]
intersectComb = intersect


{- Implementation of cv function in BitML paper 
    Extract the Contract Name / ID in a label.
    If label = Split / Withdraw / Put, return Just id
    else return Nothing (in paper: an empty set)
-}
cv :: Label -> Maybe ID
cv l =
    case l of
        Split id -> Just id
        WithDraw _ _ id -> Just id
        PutReveal _ _ id -> Just id
        _ -> Nothing


{- An union operator by using cv function:
    1st parameter 'union' (labels in 2nd param: != Split / WithDraw / Put) 
    'union' (labels from 2nd param: where there is no label with shared 
    contract ID with those from the 1st param)  
-}

cvUnionComb :: [Label] -> [Label] -> [Label]
cvUnionComb xs1 xs2 =
    let idList1 = map cv xs1 in      -- [cv(label)]
        let r2CVEmpty = filter (\x -> cv x == Nothing) xs2 in     -- cs(label) = empty  
            let r2CV = xs2 \\ r2CVEmpty in              -- cv(label) != empty, label = WithDraw / Split / Put
                let r2Exclude = filter (\x -> null ([cv x] `intersect` idList1)) r2CV in
                    union xs1 (r2CVEmpty `union` r2Exclude)



{- descendant function: generating new ids for descendant contract / deposite,
    after applying a label. Assumes, only Split / PutReveal / WithDraw can be 
    applied in this function. Otherwise: error.
-}
desc :: Label -> [Maybe ID]
desc label = 
    case label of
        Split id         -> [Just id, Nothing]  -- need info from the contract (number of sub-contracts and coins division)
        PutReveal _ _ id -> [Just id]
        WithDraw _ _ id  -> [Just id]
        _                -> errorBadArgument    -- error


{- inRun: check if a label in a run
    if not then append label in strategy
    TODO: connect with desc() 
-}
inRun :: Label -> Run -> Bool
inRun _ (Run []) = False
inRun l (Run xs) = any (\(label, _) -> label == l) xs



main = do
    -- Example of honest user (A') strategy in scissor-rock-paper example
    let labelC1 = [Split (ID "x"), WithDraw (P "A") (BCoins 2) (ID "x1")]
    let labelC2 = [Split (ID "x"), AuthReveal (P "A") (Secret "a" 2), PutReveal [] [Secret "a" 2] (ID "x2"), WithDraw (P "A") (BCoins 2) (ID "x2'")]
    let labelC3 = [Split (ID "x"), AuthReveal (P "A") (Secret "a" 2), PutReveal [] [Secret "a" 2, Secret "b" 2] (ID "x2"), Split (ID "x3"), WithDraw (P "A") (BCoins 1) (ID "x4")]

    print $ unionComb (unionComb labelC1 labelC2) labelC3               -- [all in also very bad, leave the advisor a chance to select]
    print $ intersectComb (intersectComb labelC1 labelC2) labelC3       -- []
    print $ cvUnionComb ( cvUnionComb labelC1 labelC2) labelC3          -- Difference with union:  cvUnion missing PutReveal [] [Secret "a" 2,Secret "b" 2] (ID "x2") 

    -- test desc function
    print $ desc (Control (ID "x1"))

