import Data.Char


--------- MTF algo

initialList :: [Int]
initialList = [0..255]

indexOf :: Int -> [Int] -> Int
indexOf i (h:t) = if i == h then
                    0
                  else 
                    1 + indexOf i t

removeNumArray :: Int -> [Int] -> [Int]
removeNumArray _ [] = []
removeNumArray num (h:t) = if num /= h then
                              h : removeNumArray num t
                            else 
                              removeNumArray num t

moveToFront :: Int -> [Int] -> [Int]
moveToFront num l = num : l


encode :: [Char] -> [Int]
encode [] = []
encode l = fst (foldl (\(placeholder,update) x -> (placeholder++[indexOf (ord x) update], moveToFront (ord x) (removeNumArray (ord x) update))) ([], initialList) l)

-----------------------------------------------------------------------

-- 5 == [True, False, True] 101
-- 6 == [True, True, False] 110

type Bin = [Bool]

toBin :: Int -> Bin
toBin 0 = []
toBin i = if i `mod` 2 == 1 then
                toBin (i `div` 2) ++ [True]
            else 
                toBin (i `div` 2) ++ [False]



fromBin :: Bin -> Int
fromBin l = foldl (\x e-> 2 * x + (if e then 1 else 0)) 0 l

getNumBehind1 :: [Bool] -> [Bool]
getNumBehind1 [] = []
getNumBehind1 (h : t) = t

generateFalseList :: Int -> [Bool]
generateFalseList 0 = []
generateFalseList n = False : generateFalseList (n - 1)


toElias :: Int -> Bin
toElias n = generateFalseList (length (getNumBehind1 (toBin (length (toBin n))))) ++ toBin (length (toBin n)) ++ getNumBehind1 (toBin n)


decodeEliasToNums :: Bin -> [Int]
decodeEliasToNums arr = [fromBin arr]

firstCounter :: Bin -> Int
firstCounter (h:t) = if h then
                      0
                    else
                      1 + firstCounter t

sliceList :: Int -> Int -> Bin -> Bin
sliceList from to arr = take (to - from + 1) (drop from arr)

secondCounter :: Bin -> Int
secondCounter arr = fromBin (sliceList 0 (firstCounter arr) (dropWhile (==False) arr))

getFinalNumber :: Bin -> Int
getFinalNumber arr = fromBin ([True] ++ (drop (firstCounter arr + 1) (dropWhile (==False) arr)))

decodeEliasString :: Bin -> Int
decodeEliasString arr = getFinalNumber arr


main :: IO ()
main = do 
        hamlet <- readFile "hamlet1.txt"
        --generate new sizeprint (foldl (\acc x -> acc + length (toElias x)) 0 (encode hamlet))
        --decode binary elias to numbs : print (foldl (\arr x -> arr ++ decodeEliasToNums (toElias x)) [] (encode hamlet))
        --decode elias print (foldl (\arr x -> if x == 0 then arr ++ [0] else arr ++ [decodeEliasString (toElias x)]) [] (encode hamlet))