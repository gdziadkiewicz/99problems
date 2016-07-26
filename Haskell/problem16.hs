-- Problem 16
-- (**) Drop every N'th element from a list.

-- Example:

-- * (drop '(a b c d e f g h i k) 3)
-- (A B D E G H K)
-- Example in Haskell:

-- *Main> dropEvery "abcdefghik" 3
-- "abdeghk"
-- Solutions

dropEvery xs n =
    let dropEvery' result (x:xs) n i =
            if i `mod` n == 0 then
                dropEvery' result xs n (i+1)
            else
                dropEvery' (result++[x]) xs n (i+1)
        dropEvery' result [] _ _ = result
    in dropEvery' [] xs n 1

ints = iterate (1+) 1

dropEvery' xs n = map snd $ filter (\(i, _) -> i `mod` n /= 0 ) $ zip ints xs