module Main where

import Control.Monad (void)
import Data.List (find)

data Cat = Cat {
  name :: String,
  breed :: String,
  age :: Int,
  likesTreats :: Bool,
  adopted :: Bool
} deriving (Show, Eq)

data Visitor = Visitor {
    visitorId :: Int,
    visitorName :: String,
    catInterest :: [String] 
} deriving (Show, Eq)

type Cafe = ([Cat], [Visitor])

initialCafe :: Cafe
initialCafe = ([], [])

addCat :: Cat -> Cafe -> Cafe
addCat cat (cats, visitors) = (cat:cats, visitors)

addVisitor :: Visitor -> Cafe -> Cafe
addVisitor visitor (cats, visitors) = (cats, visitor:visitors)

findSuitableCats :: Visitor -> Cafe -> [Cat]
findSuitableCats visitor (cats, _) = filter (\cat -> breed cat `elem` catInterest visitor && not (adopted cat)) cats

adoptCat :: Cat -> Visitor -> Cafe -> Maybe Cafe
adoptCat cat visitor (cats, visitors) 
    | cat `elem` cats && not (adopted cat) = 
        let updatedCat = cat { adopted = True }
            newCats = map (\c -> if c == cat then updatedCat else c) cats
        in Just (newCats, visitors)
    | otherwise = Nothing

main :: IO ()
main = do
    putStrLn "Welcome to Redacted Cat Cafe!"
    loop initialCafe

loop :: Cafe -> IO ()
loop cafe = do
    putStrLn "\nWhat would you like to do?\n1. Add Cat\n2. Add Visitor\n3. Find Cats for Adoption\n4. Adopt Cat\n5. Exit"
    choice <- getLine
    case choice of
        "1" -> addCatIO cafe >>= loop
        "2" -> addVisitorIO cafe >>= loop
        "3" -> findCatsIO cafe >>= loop
        "4" -> adoptCatIO cafe >>= loop
        "5" -> putStrLn "Sayonara meowflower \n\t\t ~ from the Redacted Cat Cafe!"
        _   -> do
            putStrLn "Invalid option, please try again."
            loop cafe

addCatIO :: Cafe -> IO Cafe
addCatIO cafe = do
    putStrLn "Enter cat's name:"
    name <- getLine
    putStrLn "Enter cat's breed:"
    breed <- getLine
    putStrLn "Enter cat's age:"
    age <- readLn
    putStrLn "Does the cat like treats? (True/False):"
    likesTreats <- readLn
    let newCat = Cat name breed age likesTreats False
    return $ addCat newCat cafe

addVisitorIO :: Cafe -> IO Cafe
addVisitorIO cafe = do
    putStrLn "Enter visitor's ID:"
    visitorId <- readLn
    putStrLn "Enter visitor's name:"
    visitorName <- getLine
    putStrLn "Enter interested cat breeds (comma-separated):"
    catInterest <- words <$> getLine
    let newVisitor = Visitor visitorId visitorName catInterest
    return $ addVisitor newVisitor cafe

findCatsIO :: Cafe -> IO Cafe
findCatsIO cafe = do
    putStrLn "Enter visitor's ID to find suitable cats:"
    inputVisitorId <- readLn 
    case filter (\v -> visitorId v == inputVisitorId) (snd cafe) of
        (v:_) -> do
            let suitableCats = findSuitableCats v cafe
            if null suitableCats
                then putStrLn "No suitable cats available for this visitor."
                else mapM_ print suitableCats
            return cafe
        _ -> do
            putStrLn "Visitor not found."
            return cafe

adoptCatIO :: Cafe -> IO Cafe
adoptCatIO cafe = do
    putStrLn "Enter cat's name to adopt:"
    catName <- getLine
    putStrLn "Enter visitor's ID:"
    inputVisitorId <- readLn 
    case (find (\c -> name c == catName) (fst cafe), find (\v -> visitorId v == inputVisitorId) (snd cafe)) of
        (Just cat, Just visitor) -> case adoptCat cat visitor cafe of
            Just newCafe -> do
                putStrLn $ "Congratulations! " ++ catName ++ " has been adopted by " ++ visitorName visitor ++ "!"
                return newCafe
            Nothing -> do
                putStrLn "Adoption not possible. Is that cat available?"
                return cafe
        _ -> do
            putStrLn "Either cat or visitor not found."
            return cafe
