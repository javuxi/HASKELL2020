functionF :: Int -> Int -> Int
functionF l u = (l + u) `div` 2

functionB :: Int -> Int -> Int
functionB l u = (l+u) - (l + u) `div` 2

rowSplitting :: String -> Int -> Int -> Int
rowSplitting (h:[]) lower upper = if h == 'F' then lower else upper
rowSplitting (h:t) lower upper = case h of
                                        'F' -> do
                                                rowSplitting t lower (functionF lower upper)
                                        'B' -> do
                                                rowSplitting t (functionB lower upper) upper

columnSplitting :: String -> Int -> Int -> Int
columnSplitting (h:[]) left right = if h == 'L' then left else right
columnSplitting (h:t) left right = case h of
                                        'L' -> do
                                                columnSplitting t left (functionF left right)
                                        'R' -> do
                                                columnSplitting t (functionB left right) right

genStrings :: String -> Int -> String
genStrings str 0 = take 7 str
genStrings str 1 = drop 7 str

generateResult :: String -> Int
generateResult str = (rowSplitting (genStrings str 0) 0 127) * 8 + (columnSplitting (genStrings str 1) 0 7)

getHighestSeatID :: [String] -> Int
getHighestSeatID arr = maximum (foldl (\acc x -> (generateResult x):acc) [] arr)

main :: IO ()
main = do
        putStrLn "Enter file path:"
        filePath <- getLine
        content <- readFile filePath
        let splitted = lines content
        putStrLn (show (getHighestSeatID (splitted)))
        return ()
