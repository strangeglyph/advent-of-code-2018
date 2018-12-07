import Data.List

main = do
    input <- readFile "input"
    let solution = solveA input
    putStrLn $ solution

solveA :: String -> String
solveA = show . checksum . lines

checksum :: [String] -> Int
checksum labels = (countN 3 labels) * (countN 2 labels)

countN :: Int -> [String] -> Int
countN n = length . filter (elem n) . map letterCounts

letterCounts :: String -> [Int]
letterCounts = map length . group . sort
