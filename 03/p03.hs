import Data.Maybe
import Data.Char
import Data.Char

sumPair :: (Int, Int) -> Int
sumPair (e1, e2) = e1 + e2


concatPair :: (String, Int) -> String
concatPair (s, n) = s ++ (show n)

-- Call me Maybe
fib :: Int -> Maybe Int
fib n = if n < 1 then
            Nothing
        else 
            if n < 3 then
                Just 1
            else
                Just (fromJust (fib (n-1)) + fromJust (fib (n-2)))

add1ToMaybe :: Maybe Int -> Maybe Int
add1ToMaybe Nothing = Nothing
add1ToMaybe (Just n) = Just (n + 1)

maybeMap :: (Int -> Int) -> Maybe Int -> Maybe Int
maybeMap _ Nothing = Nothing
maybeMap f (Just n) = Just (f n)

--List
lengthNew :: [Int] -> Int
lengthNew [] = 0
lengthNew l = 1 + lengthNew (tail l) 

foldN :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldN f acc [] = acc
foldN f acc (h:t) = foldN f (f acc h) t


filterN :: (Int -> Bool) -> [Int] -> [Int]
filterN f [] = []
filterN f (h:t) = if f h then
                    h : filterN f t
                else
                    filterN f t

filterNFold :: (Int -> Bool) -> [Int] -> [Int]
filterNFold f [] = []
filterNFold f (h:t) = foldl (\arr x -> if (f x) then arr++[x] else arr) [] t

remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove i l = filter (\x -> x /= i) l


----MTF algo

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