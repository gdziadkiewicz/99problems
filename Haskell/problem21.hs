-- Problem 21
-- Insert an element at a given position into a list.

-- Example:

-- * (insert-at 'alfa '(a b c d) 2)
-- (A ALFA B C D)
-- Example in Haskell:

-- P21> insertAt 'X' "abcd" 2
-- "aXbcd"
-- Solutions

insertAt c xs 1 =  c:xs
insertAt c (x:xs) n = x:insertAt c xs (n-1)
insertAt _ _ _ = error "n bigger than list length"