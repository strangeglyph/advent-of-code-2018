import Data.List

main = do
    input <- readFile "input"
    putStrLn $ solveA input
    putStrLn $ solveB input

solveA :: String -> String
solveA = show . checksum . lines

solveB :: String -> String
solveB = uncurry same . findMatching . lines

findMatching :: [String] -> (String,String)
findMatching = head . filter (uncurry isSimilar) . pairs

isSimilar :: String -> String -> Bool
isSimilar a b = numDifferent == 1
    where numDifferent = length $ different a b

same :: String -> String -> String
same [] [] = []
same (x:xs) (y:ys)
    | x == y    = x : same xs ys
    | otherwise = same xs ys

different :: String -> String -> [(Char, Char)]
different [] [] = []
different (x:xs) (y:ys)
    | x /= y    = (x,y) : different xs ys
    | otherwise = different xs ys

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = map (\y -> (x,y)) xs ++ pairs xs

checksum :: [String] -> Int
checksum labels = (countN 3 labels) * (countN 2 labels)

countN :: Int -> [String] -> Int
countN n = length . filter (elem n) . map letterCounts

letterCounts :: String -> [Int]
letterCounts = map length . group . sort
