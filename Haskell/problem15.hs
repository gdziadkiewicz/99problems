--  Problem 15
-- (**) Replicate the elements of a list a given number of times.

-- Example:

-- * (repli '(a b c) 3)
-- (A A A B B B C C C)
-- Example in Haskell:

-- > repli "abc" 3
-- "aaabbbccc"
-- Solutions

-- repli [] _ = []
-- repli xs n = foldr (++) [] (replicate n xs)

-- repli' xs 0 = []
-- repli' xs n = xs ++ (repli' xs (n-1))

repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n
