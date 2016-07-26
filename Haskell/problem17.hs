-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given.

-- Do not use any predefined predicates.

-- Example:

-- * (split '(a b c d e f g h i k) 3)
-- ( (A B C) (D E F G H I K))
-- Example in Haskell:

-- *Main> split "abcdefghik" 3
-- ("abc", "defghik")
-- Solutions
-- {-# LANGUAGE ViewPatterns #-}
-- split xs ( (0>) -> True) = error "n must be non-negative"
split xs n =
    let g r xs 0 = (r, xs)
        g r (x:xs) n = g (r++[x]) xs (n-1)
        g _ _ _ = error "The list is too short"
    in g [] xs n

