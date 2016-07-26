-- Problem 11
-- (*) Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

-- Example:

-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell:

-- P11> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']
-- Solutions

data MixedType = Multiple Int Char | Single Char
    deriving (Eq, Show)

encodeModified [] = []
encodeModified (x:xs) =
    let encode answer current [] = answer ++ [current]
        encode answer (Single c) (x:xs) =
            if x == c then
                encode answer (Multiple 2 c) xs
            else
                encode (answer++[Single c]) (Single x) xs
        encode answer (Multiple l c) (x:xs) =
            if x == c then
                encode answer (Multiple (l+1) c) xs
            else
                encode (answer++[Multiple l c]) (Single x) xs
    in encode [] (Single x) xs

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