import Debug.Trace


main = do
    input <- readFile "input"
    let solution = solveInstance $ init $ lines input
    putStrLn $ show solution


solveInstance :: [String] -> Integer
solveInstance = foldr ((+) . inputToInt) 0


inputToInt :: String -> Integer
inputToInt ('+':num) = read num
inputToInt num = read num
