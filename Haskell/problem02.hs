-- 2 Problem 2
-- (*) Find the last but one element of a list.

-- (Note that the Lisp transcription of this problem is incorrect.)

-- Example in Haskell:

-- Prelude> myButLast [1,2,3,4]
-- 3
-- Prelude> myButLast ['a'..'z']
-- 'y'
-- Solutions

myButLast [] = error "no last but least fo lists of lenght smaller than 2"
myButLast [_] = error "no last but least fo lists of lenght smaller than 2"
myButLast [x,_] = x
myButLast (x:xs) = myButLast xs

myButLast' = last . init
