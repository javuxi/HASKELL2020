import Data.Map (Map, lookup, fromList)

l = [x | x <- [1..100], mod x 5 == 0, mod x 7 /= 0]

p = [(i,j) | i <- [1..100], j <- [1..100], i < j]


--- qsort ---
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (pivot:t) = (qsort [x | x <- t, x <= pivot]) 
                  ++ [pivot] ++ 
                  (qsort [x | x <- t, x > pivot])

--- laziness ---
loop n = if n == 5 then 1 else loop n
lLoop = length [4, 2, loop 4, loop 5, loop 10]

{-- 
square x = x * x

fst1 (a,b) = a

--- instant evaluation ---
fst1 (5 + 4, square 6)

fst1 (9, square 6)
fst1 (9, 6 * 6)
fst1 (9, 36)
9

--- laziness ---
fst1 (5+4, square 6)
5 + 4
9
--}

--- Infinite lists ---
ones = 1 : ones

nats = 1 : (map (+1) nats)

evens = filter (\x -> mod x 2 == 0) nats

isPrime n = all (\x -> (mod n x) /= 0) [2..k]
            where
                k = floor (sqrt (fromIntegral n))

primes = filter isPrime [2..]

-- Sieve Of Eratosthenes
sieve (p:t) = p:(sieve (filter (\x -> mod x p /= 0) t))
primes1 = sieve [2..]

fibs = 1:1:(zipWith (+) fibs (tail fibs))

--- all strings a-z ---
allStrings = [x : s | s <- "":allStrings, x <- ['a'..'z']]

-- Rational numbers
data Rat = R Int Int deriving Show

allRationals = [R ste (dSum - ste) | dSum <-[2..], ste <- [1..(dSum -1)], gcd ste (dSum - ste) == 1]