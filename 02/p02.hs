rangeMap :: Int -> Int -> (Int -> String) -> String
rangeMap s e f = if s>e then
                    ""
                 else
                    (f s) ++ (rangeMap (s+1) e f)

line :: Int -> Int -> String
line n flag = if flag == 0 then
                rangeMap 1 n (\x -> if even x then
                                    "*"
                                else
                                    " ")
            else
                rangeMap 1 n (\x -> if odd x then
                                    "*"
                                else
                                    " ")


chessboard :: Int -> String
chessboard n = if even n then
                rangeMap 1 n (\x -> line n 0 ++ "\n")
            else if odd n then
                rangeMap 1 n (\x -> if odd n then
                                        line n 1 ++ "\n"
                                    else
                                        line n 0 ++ "\n")
            else 
                ""

            


type Set = Int -> Bool

singleton :: Int -> Set
singleton n = \x -> x == n

union :: Set -> Set -> Set
union set1 set2 = \x -> set1 x || set2 x

intersection :: Set -> Set -> Set
intersection set1 set2 = \x -> set1 x && set2 x

a :: Set
a = \x -> (x > 5 && x < 20)

showSet :: Int -> Int -> Set -> String
showSet s e set1 = if s > e then
                    ""
                else
                    if (set1 s) then
                        show s ++ " " ++ (showSet (s+1) e set1)
                    else
                        (showSet (s+1) e set1)

{-
showSet 6 15 a -- "6 7 8 9 10 11 12 13 14 15 "
showSet 1 7 a -- "6 7 "
showSet 18 26 a -- "18 19 "
-}

type RealF = Float -> Float

b :: RealF
b = \x -> x + 1

tableF :: Float -> Float -> Float -> RealF -> String
tableF start end step f = if start > end then
                            ""
                        else
                            (show (f start) ++ " | " ++ show (f start) ++  "\n")
                            ++ (tableF (start+step) end step f)

c :: RealF
c = \x -> 2 * x

derive :: RealF -> RealF
derive func = \x -> ((func (x+0.001) - (func x)) / ((x+0.001)-x))
