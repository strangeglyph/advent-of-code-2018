import Data.Char
import Control.Monad
import Debug.Trace

main = do
    input <- liftM init $ readFile "input"
    putStrLn $ "a: " ++ (solveA input)
    putStrLn $ "b: " ++ (solveB input)

solveA :: String -> String
solveA = show . length . fix contract

solveB :: String -> String
solveB = undefined

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
