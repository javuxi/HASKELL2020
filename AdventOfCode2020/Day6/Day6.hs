import Data.List.Split 
import Data.List

customLineSplitPart1 :: String -> [String]
customLineSplitPart1 str = splitOn "\n\n" str

customLineSplitPart2 :: [String] -> [String]
customLineSplitPart2 str = foldl (\arr x -> (customLineSplitPart3 (splitOn "\n" x)):arr) [] str

customLineSplitPart3 :: [String] -> String
customLineSplitPart3 str = drop 1 (foldl (\arr x ->  " " ++ x ++ arr) [] str)

removeSpace :: String -> String
removeSpace [] = []
removeSpace (' ':t) = removeSpace t
removeSpace (h:t) = h : removeSpace t

countUnique :: [String] -> Int
countUnique str = foldl (\acc x -> acc + (length (removeSpace (nub x)))) 0 str

main :: IO ()
main = do
        putStrLn "Enter file path:"
        filePath <- getLine
        content <- readFile filePath
        putStrLn (show (countUnique (customLineSplitPart2 (customLineSplitPart1 content))))
        return ()