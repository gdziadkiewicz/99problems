-- Problem 25
-- Generate a random permutation of the elements of a list.
--
-- Example:
--
-- * (rnd-permu '(a b c d e f))
-- (B A D C E F)
-- Example in Haskell:
--
-- Prelude System.Random>rnd_permu "abcdef"
-- Prelude System.Random>"badcef"

import System.Random

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ y:zs | (y,ys) <- select xs, zs <- permutations ys]
  where select []     = []
        select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

rnd_permu :: [a] -> IO [a]
rnd_permu xs =
  do
    let xss = permutations xs
    let l = factorial (length xs)
    i <- getStdRandom ( randomR(0, l-1) )
    return (xss !! i)

main :: IO ()
main = return ()
