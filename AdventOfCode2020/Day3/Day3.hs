getResult :: String -> Int
getResult [] = 0
getResult "." = 0
getResult "#" = 1
getResult (h:t) = (getResult [h]) + (getResult t)

getOptions :: Int -> Int -> Int -> [(Int, Int)]
getOptions x y len = [ (x,y) | (x,y) <- zip [0,x..] [0,y..(len - 1)]]

checkpath :: [String] -> [(Int, Int)] -> Int -> Int
checkpath arr options width = getResult (foldl (\acc opt -> (arr !! (snd opt) !! ((fst opt) `mod` width)):acc) [] options)

main :: IO ()
main = do
        putStrLn "Enter file path:"
        filePath <- getLine
        content <- readFile filePath
        let splittedLines = lines content
        putStrLn (show (checkpath splittedLines (getOptions 3 1 (length splittedLines)) ((length (splittedLines!!0)))))
        return()