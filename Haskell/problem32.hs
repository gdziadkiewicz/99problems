-- Problem 32
-- (**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
--
-- Example:
--
-- * (gcd 36 63)
-- 9
-- Example in Haskell:
--
-- [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]
-- Solutions

import           Test.QuickCheck

main = do
   quickCheck ((\x y -> gcd x y == myGCD x y)::Int->Int->Bool)
   return ()

myGCD x y =
  myGCD' x' y'
  where x'' = abs x
        y'' = abs y
        x' = max x'' y''
        y' = min x'' y''
        myGCD' x 0 = x
        myGCD' x y = case x `mod` y of
          0 -> y
          r -> myGCD' y r


