import Data.Text (Text, pack)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Set (Set, fromList, empty, union, intersection, size)
import qualified Data.Set as Set
import Debug.Trace
import Control.Monad

main = do
    input <- readFile "input"
--    putStrLn $ "a: " ++ (solveA input)
    putStrLn $ "b: " ++ (solveB input)

solveA :: String -> String
solveA input = show $ size $ foldr addPoints empty $ filter idNeq $ liftM2 (,) lines' lines'
    where lines' = map parseLine $ lines input
          addPoints = union . (uncurry intersect)
          idNeq = \(a,b) -> (Main.id a) /= (Main.id b)

solveB :: String -> String
solveB input = show $ Main.id $ head $ walk lines' hasEmptyIntersection
    where lines' = map parseLine $ lines input


hasEmptyIntersection :: [Line] -> Line -> [Line] -> Bool
hasEmptyIntersection xs x xs' = Set.null $ foldr union empty intersections
    where intersections = map (intersect x) (xs ++ xs')

walk :: [a] -> ([a] -> a -> [a] -> Bool) -> [a]
walk [] _ = []
walk (x:xs) f = walk' [] x xs f

walk' :: [a] -> a -> [a] -> ([a] -> a -> [a] -> Bool) -> [a]
walk' before x [] f
    | f before x []     = [x]
    | otherwise         = []
walk' before x xs f
    | f before x xs = x : walk' (x:before) (head xs) (tail xs) f
    | otherwise     = walk' (x:before) (head xs) (tail xs) f


parseLine :: String -> Line
parseLine = unwrap . parse lineRule . pack

unwrap :: IResult Text Line -> Line
unwrap (Done _ line)    = line
unwrap (Partial f)      = unwrap $ f $ pack ""
unwrap (Fail _ _ err)   = error err

lineRule :: Parser Line
lineRule = do
    char '#'
    id <- decimal
    string $ pack " @ "
    offsetX <- decimal
    char ','
    offsetY <- decimal
    string $ pack ": "
    width <- decimal
    char 'x'
    height <- decimal
    return $ Line id offsetX offsetY width height

intersect :: Line -> Line -> Set Point
intersect a b = intersection (contained a) (contained b)

contained :: Line -> Set Point
contained line = fromList [(x,y) | x <- [loX..hiX], y <- [loY..hiY]]
    where loX = offsetX line
          hiX = rightBorder line - 1
          loY = offsetY line
          hiY = bottomBorder line - 1



rightBorder :: Line -> Int
rightBorder l = width l + offsetX l

bottomBorder :: Line -> Int
bottomBorder l = height l + offsetY l

data Line = Line {
    id :: Int,
    offsetX :: Int,
    offsetY :: Int,
    width :: Int,
    height :: Int
} deriving (Show)

type Point = (Int, Int)
