import Data.List.Split

options = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

customLineSplitPart1 :: String -> [String]
customLineSplitPart1 str = splitOn "\n\n" str

customLineSplitPart2 :: [String] -> [String]
customLineSplitPart2 str = foldl (\arr x -> (customLineSplitPart3 (splitOn "\n" x)):arr) [] str

customLineSplitPart3 :: [String] -> String
customLineSplitPart3 str = drop 1 (foldl (\arr x ->  " " ++ x ++ arr) [] str)

hasCID :: [String] -> Bool
hasCID str = "cid" `elem` str

isLegit :: [String] -> Bool
isLegit arr = if (length arr) < ((length options) - 1) then
                False
            else if (length arr) == ((length options) - 1) then
                not (hasCID arr)
            else
                True

splitThird :: [String] -> [String]
splitThird arr = foldl (\acc x-> ((splitOn ":" x)!!0):acc) [] arr

splitFirst :: [String] -> [[String]]
splitFirst arr = foldl (\acc x -> (splitOn " " x):acc) [] arr

splitSecond :: [[String]] -> [[String]]
splitSecond arr = foldl (\acc x -> (splitThird x):acc) [] arr

countLegit :: [[String]] -> Int
countLegit arr = foldl (\acc x -> if isLegit x then acc + 1 else acc) 0 arr

main :: IO ()
main = do
        putStrLn "Enter file path:"
        filePath <- getLine
        content <- readFile filePath
        let splitted = customLineSplitPart2 (customLineSplitPart1 content)
        putStrLn (show (countLegit (splitSecond (splitFirst (splitted)))))
        return ()