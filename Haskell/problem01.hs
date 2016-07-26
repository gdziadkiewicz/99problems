-- Problem 1
-- (*) Find the last element of a list.

-- (Note that the Lisp transcription of this problem is incorrect.)

-- Example in Haskell:

-- Prelude> myLast [1,2,3,4]
-- 4
-- Prelude> myLast ['x','y','z']
-- 'z'
myLast' [] = error "no last for empty list"
myLast' (x:xs)
    |xs == [] = x
    |otherwise = myLast xs 

myLast [] = error "no last for empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast'' = head.reverse

myLast''' = last