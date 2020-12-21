import System.Environment
import Data.Ord
import Data.List

type Board = [[Bool]]
type Rule = [Bool]  -> Bool

bSize = 10

showBoard :: Board -> String
showBoard [] = "\n"
showBoard (h:t) = (map (\p-> if p then '#' else '.') h)++"\n"++(showBoard t)


change :: Board -> Rule -> (Int,Int) -> Bool
change b rule (x,y) = let
                  pos = [(x-diff1, y-diff2) | diff1 <- [-1,0,1], diff2 <- [-1,0,1]]
                  vals = map (getV b) pos
                  getV b (x,y) = if x>=0 && y>=0 && x<bSize &&y<bSize then
                                  (b!!x)!!y
                                 else 
                                  False
                in
                  rule vals

step :: Rule -> Board -> Board
step rule b =
  let 
    pos = map (\x->[(x,y)|y<-[0..bSize-1]]) [0..bSize-1]
  in
    map (\l-> map (change b rule) l) pos


run :: Rule -> Board -> [Board]
run r b = let
  runAux b' trace = if elem b' trace then
                    []
                  else 
                    b':(runAux (step r b') (b':trace))
  in 
    runAux b []

createRule :: [Int] -> [Int] -> Rule
createRule born alive = \p -> case p!!4 of
                                False -> elem (countLive p) born
                                True -> elem (countLive p) alive
                            where
                                countLive = length . filter (==True)

conway :: Rule
conway = createRule [3] [3,4]

subsets :: [a] -> [[a]]
subsets [x] = [[x]]
subsets (h:t) = subsets [h] ++ subsets t ++ map (h:) (subsets t)

allRules :: [Rule]
allRules = [createRule b a | b <- (subsets [1..8]), a <- (subsets [1..8])]

mostInteresting :: [Rule] -> Rule
mostInteresting r = let
                        t = rTab 4234723
                    in
                        snd (maximumBy (comparing fst) (foldl (\acc x -> (length (run x t), x):acc) [] r))

---- generirajmo naključna začetna polja ----
genRandom :: Int -> Int
genRandom seed = mod ((2234455323*seed)+3453434)  34534535

randList :: Int -> Int -> [Int]
randList _ 0 = [] 
randList seed len =let
                    r = genRandom seed
                   in
                      r : (randList r (len -1))

randRangeList :: Int -> Int -> Int -> [Int]
randRangeList l h  len = map (\x->(mod x (h-l)) + l ) (randList 34534335 len)

groupLen :: [a] -> Int -> [[a]]
groupLen [] _ = []
groupLen l n = (take n l ) : groupLen (drop n l) n


rTab seed = groupLen (map (\x->mod x 2 == 0)(randList seed (bSize^2))) bSize

showTrace :: [Board] -> IO ()
showTrace [] = return ()
showTrace (h:t) = do
                    putStrLn $ showBoard h
                    getLine
                    showTrace t

main :: IO ()
main = do
        args <- getArgs 
        let seed = if length args == 1 then (read $ head args :: Int) else 356237427
        print (map (\r->length $ run r (rTab seed)) (take 3 allRules))