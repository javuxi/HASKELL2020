import Data.List.Split

customLineSplitPart1 :: [String] -> [(String, Int)]
customLineSplitPart1 arr = reverse (foldl (\acc x -> (customLineSplitPart2 x):acc) [] arr)

customLineSplitPart2 :: String -> (String, Int)
customLineSplitPart2 str = ((splitOn " " str)!!0, (if take 1 ((splitOn " " str)!!1) == "-" then ((read (drop 1 ((splitOn " " str)!!1)) :: Int) * (-1)) else (read (drop 1 ((splitOn " " str)!!1)) :: Int)))

getCounter :: [(String, Int)] -> [Int] -> Int -> Int -> Int -> Int
getCounter arr visited acc position len = if any (position==) visited then
                                            acc
                                        else
                                            case (fst (arr!!position)) of
                                                "nop" -> do
                                                            getCounter arr (position:visited) acc (abs((position + 1) `mod` (len - 1))) len
                                                "jmp" -> do
                                                            getCounter arr (position:visited) acc (abs((position + (snd (arr!!position)))) `mod` (len - 1)) len
                                                "acc" -> do
                                                            getCounter arr (position:visited) (acc + (snd (arr!!position))) ((position + 1) `mod` (len - 1)) len

main :: IO ()
main = do
        putStrLn "Enter file path:"
        filePath <- getLine
        content <- readFile filePath
        let len = length (lines content)
        let splitted = customLineSplitPart1 (lines content)
        putStrLn (show (getCounter (splitted) [] 0 0 len))
        return ()