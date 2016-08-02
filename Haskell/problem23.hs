-- Problem 23
-- Extract a given number of randomly selected elements from a list.
--
-- Example:
--
-- * (rnd-select '(a b c d e f g h) 3)
-- (E D A)
-- Example in Haskell:
--
-- Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
-- eda
-- Solutions

import System.Random
import Control.Monad

rnd_select :: [a] -> Int -> IO [a]
rnd_select str n =
      let randomIndex = getStdRandom (randomR (0, length str - 1))
          randomElement = do
            i <- randomIndex
            return (str !! i)
          in
      replicateM n randomElement
