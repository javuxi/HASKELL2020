import Data.List.Split
import Data.List

customLineSplitPart1 :: [String] -> [(String, [String])]
customLineSplitPart1 str = foldl (\acc x -> (customLineSplitPart2 x, customLineSplitPart3 x):acc) [] str

customLineSplitPart2 :: String -> String
customLineSplitPart2 str = (splitOn " bags contain " str)!!0

customLineSplitPart3 :: String -> [String]
customLineSplitPart3 str = foldl (\acc x -> if x == "no other bags." then acc else (removeRedundantStr (drop 2 x)):acc) [] (splitOn ", " ((splitOn " bags contain " str)!!1))

removeRedundantStr :: String -> String
removeRedundantStr str = if length (splitOn " bags." str) == 2 then 
                            (splitOn " bags." str)!!0 
                        else if length (splitOn " bags" str) == 2 then
                            (splitOn " bags" str)!!0
                        else 
                            (splitOn " bag" str)!!0

containsValue :: [String] -> String -> Bool
containsValue [] _ = False
containsValue (h:t) str = if h == str then
                            True
                        else
                            containsValue t str

validOptions :: [(String, [String])] -> String -> [String]
validOptions options str = let
                            legit = [fst x | x <- options, containsValue (snd x) str]
                        in 
                            legit ++ concat (map (\x -> validOptions options x) legit)

main :: IO ()
main = do
        putStrLn "Enter file path:"
        filePath <- getLine
        content <- readFile filePath
        let customSplit = customLineSplitPart1 (lines content)
        putStrLn (show (length (nub (validOptions customSplit "shiny gold"))))
        return ()