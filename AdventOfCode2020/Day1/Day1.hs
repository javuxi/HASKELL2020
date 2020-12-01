f :: [String] -> [Int]
f l = map read l

findSolution :: [Int] -> [(Int, Int)]
findSolution ints = [(x,y) | x <- ints, y <- ints, x /= y, x+y == 2020]

result :: [(Int, Int)] -> String
result (h:_) = show (fst h * snd h)

nums :: [(Int, Int)] -> String
nums (h:_) = show (fst h) ++ " " ++ show (snd h)

main :: IO ()
main = do
        putStrLn "Enter file path:"
        filePath <- getLine
        content <- readFile filePath
        let ints = f (lines content)
        let res = result (findSolution ints)
        let resNum = nums (findSolution ints)
        putStr "Nums: "
        putStrLn resNum
        putStr "Result: "
        putStrLn res
        return()