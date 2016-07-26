-- Problem 13
-- (**) Run-length encoding of a list (direct solution).

-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

-- Example:

-- * (encode-direct '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell:

-- P13> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']
-- Solutions

encode [] = []
encode (x:xs) =
    let encode' answer current [] = answer ++ [current]
        encode' answer (l, c) (x:xs) =
            if x == c then
                encode' answer (l+1, c) xs
            else
                encode' (answer++[(l, c)]) (1, x) xs
    in encode' [] (1, x) xs

encodeModified' = (map f).encode where
    f (1, c) = Single c
    f (l, c) = Multiple l c

encodeDirect = encodeModified'