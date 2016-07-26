-- 8 Problem 8
-- (**) Eliminate consecutive duplicates of list elements.

-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

-- Example:

-- * (compress '(a a a a b c c a a d e e e e))
-- (A B C A D E)
-- Example in Haskell:

-- > compress "aaaabccaadeeee"
-- "abcade"
-- Solutions

compress [] = []
compress (x:xs) =
    let compress' accumulator current (x:xs) =
            if x == current then
                compress' accumulator current xs
            else compress' (accumulator ++ [x]) x xs
        compress' a c [] = a
    in compress' [x] x xs