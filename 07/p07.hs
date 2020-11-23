data MyList a = Nil | Cons a (MyList a)

-- Either
data Bodisi a b = Levi a | Desni b deriving Show

g :: String -> Int -> Either Int String
g s x = if x == 1 then
            Right s 
        else 
            Left x

data AlterList a b = ANil | ACons a (AlterList b a) deriving Show

getNth :: AlterList a b -> Int -> Either a b
getNth (ACons h _) 0 = Left h
getNth (ACons _ l) n = case (getNth l (n - 1)) of
                        Left x -> Right x
                        Right x -> Left x

mapAlt :: (a -> c) -> (b -> d) -> AlterList a b -> AlterList c d
mapAlt _ _ ANil = ANil
mapAlt fa fb (ACons h l) = ACons (fa h) (mapAlt fb fa l)

f :: a -> a 
f x = x

data Nums = One | Two | Three deriving (Show, Eq)

data Measure = CM Int | Inch Int

instance Eq Measure where
    (==) (CM x) (CM y) = x == y
    (==) (Inch x) (Inch y) = x == y
    (==) (CM x) (Inch y) = (100 * x) == (254 * y)
    (==) (Inch x) (CM y) = (100 * y) == (254 * x)

instance Show Measure where 
    show (CM x) = (show x) ++ "cm"
    show (Inch y) = (show y) ++ "in - please use metric"

instance Ord Nums where
    (<=) One _ = True
    (<=) Two Two = True
    (<=) Two Three = True
    (<=) Three Three = True
    (<=) _ _ = False



data Rat = R Int Int

multi :: Rat -> Rat -> Int
multi (R _ d1) (R _ d2) = if d1 > d2 then
                                d1 `div` d2
                            else
                                d2 `div` d1

checkMulti :: Rat -> Rat -> Bool
checkMulti (R n1 d1) (R n2 d2) = if d1 > d2 then
                                    areSame (R (n2 * (multi (R n1 d1) (R n2 d2))) (d2 * (multi (R n1 d1) (R n2 d2)))) (R n1 d1)
                                else 
                                    areSame (R (n1 * (multi (R n1 d1) (R n2 d2))) (d1 * (multi (R n1 d1) (R n2 d2)))) (R n2 d2)

areSame :: Rat -> Rat -> Bool
areSame (R n1 d1) (R n2 d2) = if n1 == n2 && d1 == d2 then 
                                True
                            else if n1 == n2 && d1 /= d2 then
                                False
                            else if n1 /= n2 && d1 == d2 then
                                False
                            else if d1 > d2 && n2 > n1 then
                                False
                            else if d1 /= d2 && (gcd d1 d2) == 1 then
                                False
                            else
                                checkMulti (R n1 d1) (R n2 d2)

makeSame :: Rat -> Rat -> Rat
makeSame (R n1 d1) (R n2 d2) = R (n1 * d2) (d1 * d2)

isBigger :: Rat -> Rat -> Bool
isBigger (R n1 d1) (R n2 d2) = if n1 < n2 then
                                True
                            else
                                False

addUp :: Rat -> Rat -> Rat
addUp (R n1 d1) (R n2 d2) = (R (n1 + n2) d1)

instance Eq Rat where 
    (==) d1 d2 = areSame d1 d2

instance Show Rat where
    show (R n d) = "Num: " ++ show(n) ++ "/" ++ show(d)

instance Ord Rat where
    (<=) (R n1 d1) (R n2 d2) = (areSame (R n1 d1) (R n2 d2)) || (isBigger (makeSame (R n1 d1) (R n2 d2)) (makeSame (R n2 d2) (R n1 d1)))

instance Num Rat where
    (+) (R n1 d1) (R n2 d2) = addUp (makeSame (R n1 d1) (R n2 d2)) (makeSame (R n2 d2) (R n1 d1))