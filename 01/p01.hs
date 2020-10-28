evenLine :: Int -> String
evenLine k = if k == 0 then
                "\n"
            else if even k then
                "*" ++ evenLine(k-1)
            else if odd k then
                " " ++ evenLine(k-1)
            else 
                ""

oddLine :: Int -> String
oddLine k = if k == 0 then
                "\n"
            else if odd k then
                "*" ++ oddLine(k-1)
            else if even k then
                " " ++ oddLine(k-1)
            else 
                ""


multiLine :: Int -> Int -> String
multiLine lines counter = if counter == 0 then
                ""
            else 
                (evenLine lines) ++ (multiLine lines (counter-1))

oddGrid :: Int -> Int -> String
oddGrid lines counter = if counter == 0 then
                ""
            else
                if even counter then
                    (evenLine lines) ++ (oddGrid lines (counter-1))
                else
                    (oddLine lines) ++ (oddGrid lines (counter-1))


chessboard :: Int -> IO()
chessboard n = if even n then
                putStr (multiLine n n)
            else if odd n then
                putStr (oddGrid n n)
            else 
                putStr ""


helper :: Int -> Int -> Bool
helper a b = if a <= 2 then
                if a == 2 then
                    True
                else
                    False
            else if b < a then
                if a `mod` b == 0 then
                    False
                else
                    helper a (b+1)
            else 
                True

isPrime :: Int -> Bool
isPrime n = helper n 2

nPrime :: Int -> [Int]
nPrime n = if n >= 2 then
            if isPrime(n) then
                n : (nPrime (n-1))
            else
                nPrime (n-1)
        else
            []