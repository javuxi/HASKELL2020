arrSplit :: [String] -> [Int]
arrSplit arr = reverse (foldl (\acc x -> (read x :: Int):acc) [] arr)

isLegit :: [Int] -> Int -> Bool
isLegit arr num = if length ([x | x <- arr, y <- arr, x + y == num]) > 0 then True else False

firstNonXMAS :: [Int] -> Int -> Int -> Int
firstNonXMAS arr preamble position = if isLegit (take preamble arr) (arr!!preamble) then
                                        firstNonXMAS (tail arr) preamble (position + 1)
                                    else
                                        arr!!preamble

main :: IO ()
main = do
        putStrLn "Enter file path:"
        filePath <- getLine
        content <- readFile filePath
        let splitted = lines content
        putStrLn (show (firstNonXMAS (arrSplit splitted) 25 0))
        return()