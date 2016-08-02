-- Problem 31
-- (**) Determine whether a given integer number is prime.
--
-- Example:
--
-- * (is-prime 7)
-- T
-- Example in Haskell:
--
-- P31> isPrime 7
-- True
-- Solutions


main :: IO ()
main = putStrLn "Hello World"


isPrime :: Int -> Bool
isPrime n =
  let a = (floor . sqrt . fromIntegral) n in
  not $ any (\x -> n `mod` x == 0) [2..a]
