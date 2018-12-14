import Control.Monad
import Data.Char
import Data.List
import Debug.Trace

main = do
    input <- liftM init $ readFile "input"
    putStrLn $ "a: " ++ (solveA input)
    putStrLn $ "b: " ++ (solveB input)

solveA :: String -> String
solveA = show . length . fix contract

solveB :: String -> String
solveB input = show $ minimum $ map (length . fix contract . stripUnit reduced) ['a'..'z']
    where reduced = fix contract input

fix :: (Eq a) => (a -> a) -> a -> a
fix f x
    | f x == x  = x
    | otherwise = fix f $ f x

contract :: String -> String
contract ""     = ""
contract [x]    = [x]
contract (a:b:xs)
    | a == toUpper b && b == toLower a  = contract xs
    | b == toUpper a && a == toLower b  = contract xs
    | otherwise                         = a : contract (b:xs)

stripUnit :: String -> Char -> String
stripUnit s c = fix (delete (toUpper c) . delete (toLower c)) s
