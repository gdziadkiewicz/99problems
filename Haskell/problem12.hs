-- Problem 12
-- (**) Decode a run-length encoded list.

-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

-- Example in Haskell:

-- P12> decodeModified 
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
-- Solutions

data MixedType = Multiple Int Char | Single Char
    deriving (Eq, Show)

decodeModified [] = []
decodeModified (x:xs) =
    (decode x)++(decodeModified xs)
    where
        decode (Single c) = [c]
        decode (Multiple l c) = replicate l c