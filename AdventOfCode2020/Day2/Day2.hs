import Data.List.Split

countAppearance :: String -> Char -> Int -> Int
countAppearance [] _ counter = counter
countAppearance (h:t) symbol counter = if h == symbol then
                                        countAppearance t symbol (counter + 1)
                                    else 
                                        countAppearance t symbol counter

getMinMax :: [String] -> (Int, Int)
getMinMax str = ((read ((splitOn "-" (str!!0))!!0)::Int), (read ((splitOn "-" (str!!0))!!1)::Int))

getSymbol :: [String] -> Char
getSymbol str = (head (splitOn ":" (str!!1))!!0)

isLegit :: [String] -> Bool
isLegit str = if ((countAppearance (str!!2) (getSymbol str) 0) >= (fst (getMinMax str))) && ((countAppearance (str!!2) (getSymbol str) 0) <= (snd (getMinMax str))) then 
                True 
            else 
                False

countLegit :: [String] -> Int -> Int
countLegit [] acc = acc
countLegit (h:t) acc = if (isLegit (words h)) then 
                        countLegit t (acc + 1) 
                    else 
                        countLegit t acc

main :: IO ()
main = do
        putStrLn "Enter file path:"
        filePath <- getLine
        content <- readFile filePath
        let splitted = lines content
        let num = countLegit splitted 0
        putStrLn (show num)
        return()