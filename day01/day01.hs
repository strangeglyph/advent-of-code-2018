import Data.Set (Set,member,insert,empty)


main = do
    input <- readFile "input"
    let solution = solveInstance $ init $ lines input
    putStrLn $ show solution
    let solution2 = solve2 $ init $ lines input
    putStrLn $ show solution2


solveInstance :: [String] -> Integer
solveInstance = foldr ((+) . inputToInt) 0

solve2 :: [String] -> Integer
solve2 = findFirstDuplicate . scannedFreqs

findFirstDuplicate :: [Integer] -> Integer
findFirstDuplicate xs = findFirstDuplicate' xs empty

findFirstDuplicate' :: [Integer] -> Set Integer -> Integer
findFirstDuplicate' (x:xs) found
    | member x found    = x
    | otherwise         = findFirstDuplicate' xs $ insert x found

scannedFreqs :: [String] -> [Integer]
scannedFreqs = scanl (flip $ (+) . inputToInt) 0 . cycle

inputToInt :: String -> Integer
inputToInt ('+':num) = read num
inputToInt num = read num
