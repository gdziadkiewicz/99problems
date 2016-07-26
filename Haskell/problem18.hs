-- Problem 18
-- (**) Extract a slice from a list.

-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

-- Example:

-- * (slice '(a b c d e f g h i k) 3 7)
-- (C D E F G)
-- Example in Haskell:

-- *Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
-- Solutions

slice xs a b =
    let start = min a b 
        end = max a b
        f result _ 0 0 = result
        f result (x:xs) 0 end = f (result ++ [x]) xs 0 (end-1)
        f [] (x:xs) start end = f [] xs (start-1) (end-1)
        f _ _ 0 end = error "end is bigger than list length"
        f _ _ start _ = error "start is bigger than list length"
    in f [] xs (start-1) end
