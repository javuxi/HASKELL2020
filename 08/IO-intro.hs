makeMenu :: [[String]] -> IO ()
makeMenu phonebook = do
                        num <- chooseOptions
                        case num of
                            1 -> do
                                    content <- getContent 
                                    makeMenu content
                            2 -> do 
                                    showPhonebook phonebook 
                                    makeMenu phonebook
                            3 -> do 
                                    name <- getName
                                    findNames phonebook name
                                    makeMenu phonebook
                            4 -> do
                                    userInfo <- getInfo
                                    makeMenu (userInfo:phonebook)
                            5 -> do
                                    name <- getName
                                    let newPhonebook = removeNames phonebook name
                                    makeMenu newPhonebook
                            6 -> do
                                    path <- getPathToSave
                                    writeFile path (unlines (toString phonebook []))
                            7 -> do
                                    return ()


allOptions :: IO ()
allOptions = do
                putStrLn "1. Read phonebook from file."
                putStrLn "2. Print phonebook."
                putStrLn "3. Search by name."
                putStrLn "4. Insert a new entry."
                putStrLn "5. Remove an entry by name."
                putStrLn "6. Save to file."
                putStrLn "7. Quit."

chooseOptions :: IO Int
chooseOptions = do  
                    putStrLn "Enter a number:"
                    ns <- getLine
                    let n = (read ns)::Int
                    return n
                        
getContent :: IO [[String]]
getContent = do 
                putStrLn "Enter file path:"
                path <- getLine
                newphonebook <- readFile path
                return (makeLines [] (lines newphonebook))

makeLines :: [[String]] -> [String] -> [[String]]
makeLines l [] = l 
makeLines l (x:xx) = makeLines ((words x):l) xx

showPhonebook :: [[String]] -> IO ()
showPhonebook [] = return ()
showPhonebook ([]:xxx) = do
                            putStrLn " "
                            showPhonebook xxx
showPhonebook ((x:xx):xxx) = do
                                putStr x
                                putStr " "
                                showPhonebook (xx:xxx)

getName :: IO String
getName = do
            putStrLn "Enter a name:"
            name <- getLine
            return name

getPathToSave :: IO String
getPathToSave = do
                    putStrLn "Enter a path to save the file:"
                    name <- getLine
                    return name

getInfo :: IO [String]
getInfo = do
            putStrLn "1. Enter a name:"
            name <- getLine
            putStrLn "2. Enter a surname:"
            surname <- getLine
            putStrLn "3. Enter a phone number:"
            phoneNumber <- getLine
            return (name:surname:phoneNumber:[])
            
findNames :: [[String]] -> String -> IO ()
findNames l name = showPhonebook (filter (\(x:_:_) -> x == name) l)

removeNames :: [[String]] -> String -> [[String]]
removeNames l name = filter (\(x:_:_) -> x /= name) l

toString :: [[String]] -> [String] -> [String]
toString [] l = l
toString (x:xx) l = toString xx ((unwords x):l)

main :: IO ()
main = do
        allOptions
        makeMenu []

--- Telefonski imenik ---
-- [(ime, priimek, stevilka)] [(String, String, Int)]
{-
1. Read phonebook from file
    (readFile, lines, words)
2. Print phonebook
3. Search by name
4. Insert a new entry
5. Remove an entry (ime)
6. Save to file
    (writeFile)
7. Quit
Enter number:
-}