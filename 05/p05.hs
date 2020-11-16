import Text.Read

data Student = Student {name :: String, surname:: String, id :: Int} | 
            ExchangeStudent {name :: String, surname:: String, id :: Int, country :: String} deriving Show



data MyList = Nil | Cons Int MyList deriving Show

toList :: MyList -> [Int]
toList Nil = []
toList (Cons h t) = h : (toList t)



data Nat = Z | S Nat deriving Show

plus :: Nat -> Nat -> Nat
plus Z n = n 
plus (S n) m = S (plus n m)

times :: Nat -> Nat -> Nat
times Z n = Z
times n Z = Z
times (S Z) n = n 
times (S n) m = plus m (times n m)

fib :: Nat -> Nat
fib Z = (S Z)
fib (S Z) = (S Z)
fib (S (S n)) = plus (fib n) (fib (S n))



--a + 5 + 7 * b
data Exp = Val Int | Var String | Plus Exp Exp | Times Exp Exp deriving Show

value :: Exp -> Maybe Int
value (Val n) = Just n
value (Var _) = Nothing
value (Plus e1 e2) = case (value e1, value e2) of 
                        (Just m, Just n) -> Just (n + m)
                        _ -> Nothing
value (Times e1 e2) = case (value e1, value e2) of 
                        (Just m, Just n) -> Just (n * m)
                        _ -> Nothing



type VarDict = [(String, Int)]

searchValInList :: VarDict -> String -> Maybe Int
searchValInList [] _ = Nothing
searchValInList ((v1, val):t) m = if (m == v1) then
                                    Just val 
                                else
                                    searchValInList t m 

varValue :: Exp -> VarDict -> Maybe Int
varValue (Val n) _ = Just n
varValue (Var n) dict = searchValInList dict n
varValue (Plus e1 e2) dict = case (varValue e1 dict, varValue e2 dict) of
                                (Just m, Just n) -> Just (m + n)
                                _ -> Nothing
varValue (Times e1 e2) dict = case (varValue e1 dict, varValue e2 dict) of
                                (Just m, Just n) -> Just (m * n)
                                _ -> Nothing

-- a 5 + b 6 * + == (a+5) + (b*6)
--[]
--[Var a]
--[Val 5, Var a]
--[Plus (Val 5) (Var a)]
--[Var b, Plus (Val 5) (Var a)]
--[Val 6, Var b, Plus (Val 5) (Var a)]
--[Times (Val 6) (Var b), Plus (Val 5) (Var a)]
--[Plus (Times (Val 6) (Var b)) (Plus (Val 5) (Var a))]

genExp :: [String] -> [Exp] -> Maybe Exp
genExp [] [e] = Just e
genExp ("+":t) (e1:e2:rest) = genExp t ((Plus e1 e2) : rest)
genExp ("*":t) (e1:e2:rest) = genExp t ((Times e1 e2) : rest)
genExp (h:t) stack = case readMaybe h of
                        Just n -> genExp t ((Val n) : stack)
                        Nothing -> genExp t ((Var h) : stack)

parseString :: String -> Maybe Exp
parseString str = genExp (words str) []