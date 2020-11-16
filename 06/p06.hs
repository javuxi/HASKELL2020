data RegEx = Empty | Eps | Ch Char | Plus RegEx RegEx | Conc RegEx RegEx | Star RegEx deriving Show

nullable :: RegEx -> Bool
nullable Empty = False
nullable Eps = True
nullable (Ch _) = False
nullable (Plus r1 r2) = (nullable r1) || (nullable r2)
nullable (Conc r1 r2) = (nullable r1) && (nullable r2)
nullable (Star _) = True

derive :: RegEx -> Char -> RegEx
derive Empty _ = Empty
derive Eps _ = Empty
derive (Ch symbol) terminal = if symbol == terminal then Eps else Empty
derive (Plus r1 r2) terminal = Plus (derive r1 terminal) (derive r2 terminal)
derive (Conc r1 r2) terminal = if (nullable(r1)) then
                                Plus (Conc (derive r1 terminal) r2) (derive r2 terminal)
                            else 
                                Conc (derive r1 terminal) (r2)
derive (Star r) terminal = Conc (derive r terminal) (Star r)

genStr :: [String] -> [RegEx] -> RegEx
genStr [] [] = Empty
genStr [] [r] = r
genStr ("eps":t) stack = genStr t (Eps : stack)
genStr ("+":t) (r1 : r2 : rest) = genStr t ((Plus r1 r2) : rest)
genStr (".":t) (r1 : r2 : rest) = genStr t ((Conc r1 r2) : rest)
genStr ("*":t) (r : rest) = genStr t ((Star r) : rest)
genStr (h:t) stack = genStr t ((Ch (head h)) : stack)

fromString :: String -> RegEx
fromString str = genStr (words str) []

accept :: RegEx -> String -> Bool
accept r [] = nullable (r)
accept r (h:t) = accept (derive r h) t

generateOptions :: String -> [String]
generateOptions [] = []
generateOptions str = str : generateOptions (tail str)

generateNullableOptions :: RegEx -> [String] -> [String]
generateNullableOptions r arr = foldl (\arr x -> if (accept r x) then arr ++ [x] else arr) [] arr

zipOptions :: [String] -> [(Int, String)]
zipOptions arr = zip [1..] arr

findAll :: RegEx -> String -> [(Int, String)]
findAll r str = zipOptions (generateNullableOptions r (generateOptions str))