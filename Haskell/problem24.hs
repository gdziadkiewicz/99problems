-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.
--
-- Example:
--
-- * (rnd-select 6 49)
-- (23 1 17 33 21 37)
-- Example in Haskell:
--
-- Prelude System.Random>diff_select 6 49
-- Prelude System.Random>[23,1,17,33,21,37]
-- Solutions

import System.Random
import System.Random.Shuffle

main :: IO ()
main = return ()

diff_select :: Int -> Int -> IO [Int]
diff_select n m
  | m < n = fail "N is bigger than M"
  | m == n = return [1..m]
  | otherwise = do
    stdGen <- getStdGen
    return $ take n (shuffle' [1..m] m stdGen )
