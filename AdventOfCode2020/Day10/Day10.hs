import Data.List

arrSplit :: [String] -> [Int]
arrSplit arr = let
                    newArr = sort (reverse (foldl (\acc x -> (read x :: Int):acc) [] arr))
                in
                    (3:(zipWith (-) newArr (0:newArr)))

genResult :: [Int] -> Int
genResult arr = let
                    (x,y) = (length (filter (1==) arr), length (filter (3==) arr))
                in 
                    x * y
                    
main :: IO ()
main = do
        putStrLn "Enter file path:"
        filePath <- getLine
        content <- readFile filePath
        let splitted = arrSplit (lines content)
        putStrLn (show (genResult splitted))
        return()