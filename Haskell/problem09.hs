-- 9 Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

-- Example:

-- * (pack '(a a a a b c c a a d e e e e))
-- ((A A A A) (B) (C C) (A A) (D) (E E E E))
-- Example in Haskell:

-- *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
--              'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

pack [] = []
pack (x:xs) =
    let pack' answer current [] = answer ++ [current]
        pack' answer current (x:xs) = 
            if x == (head current) then
                pack' answer (current++[x]) xs
            else
                pack' (answer++[current]) [x] xs
        in
        pack' [] [x] xs 