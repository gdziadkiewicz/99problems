-- Problem 4
-- (*) Find the number of elements of a list.

-- Example in Haskell:

-- Prelude> myLength [123, 456, 789]
-- 3
-- Prelude> myLength "Hello, world!"
-- 13
-- Solutions

myLength' xs = length xs

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength'' = sum . map (\x -> 1)
myLength''' xs = foldr (\_ y -> y+1) 0 xs