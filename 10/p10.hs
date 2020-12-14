-- Game Of Life --

sBoard = 10
type Board = [[Bool]]
-- True if alive, false if dead
type Rule = [Bool] -> Bool

{-
xxx
x*x
xxx 
-}

conway :: Rule
conway cells = case cells!!4 of
                True -> (countLive == 3) || (countLive  == 4)
                False -> countLive == 3
              where
                countLive  = length (filter (==True) cells)

change :: Rule -> Board -> (Int, Int) -> Bool
change rule b (x,y) = let
                  pos = [(x+diff1, y+diff2) | diff1 <- [-1,0,1], diff2 <- [-1,0,1]]
                  vals = map (getV b) pos
                  getV b (x,y) = if x >= 0 && y >= 0 && x<sBoard && y<sBoard then
                                  (b!!x)!!y
                                 else 
                                  False
                in
                  rule vals



boolToSign :: [Bool] -> String
boolToSign arr = reverse (foldl (\acc x -> if x == True then '#':acc else '.':acc) [] arr)

generateBoard :: Board -> [String]
generateBoard board = reverse (foldl (\acc line -> (boolToSign line):acc) [] board)

showBoard :: Board -> String
showBoard board = unlines (generateBoard board)



placeholder :: Board
placeholder = [[False | j <- [0..sBoard-1]] | k <- [0..sBoard-1]] 

generateOptions :: [(Int, Int)]
generateOptions = [(x,y)| x <-[0..sBoard-1], y <- [0..sBoard-1]]

replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]
replace2D v (x,y) = replace y (replace x (const v))

newBoard :: Rule -> Board -> Board -> [(Int, Int)] -> Board
newBoard _ _ tmp [] = tmp
newBoard rule board tmp (h:t) = if change rule board h then
                                        newBoard rule board (replace2D True (fst h, snd h) tmp) t
                                    else
                                        newBoard rule board tmp t

step :: Rule -> Board -> Board
step rule board = newBoard rule board placeholder generateOptions



run :: Rule -> Board -> [Board] -> [Board]
run rule board tracker = if board `elem` tracker then
                            tracker
                        else 
                            run rule (step conway board) (board:tracker)

random :: Int -> Int 
random seed = ((seed * 84312383) + 9129502) `mod` 95910479

randomList :: Int -> Int -> [Int]
randomList _ 0 = []
randomList seed n = (random seed : randomList (random seed) (n-1))

genBool :: Int -> Int -> [Bool] -> [Bool]
genBool seed 0 arr = arr
genBool seed size arr = genBool (seed - (size * 17)) (size - 1) (if even seed then True:arr else False:arr)

genBoard :: Int -> Int -> Int -> [[Bool]] -> Board
genBoard seed size 0 arr = arr
genBoard seed size counter arr = genBoard (seed * 7) size (counter -1) ((genBool seed size []):arr)