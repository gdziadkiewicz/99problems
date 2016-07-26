-- (**) Flatten a nested list structure.

-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

-- Example:

-- * (my-flatten '(a (b (c d) e)))
-- (A B C D E)
-- Example in Haskell:

-- We have to define a new data type, because lists in Haskell are homogeneous.

--  data NestedList a = Elem a | List [NestedList a]
-- *Main> flatten (Elem 5)
-- [5]
-- *Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- *Main> flatten (List [])
-- []

data NestedList a = Elem a | List [NestedList a]


-- flatten (List xs) =
--     let f qs ys =
--          case ys of
--             (Elem y) -> qs ++ [y]
--             (List yss) -> qs ++ (flatten yss)
--     in
--     foldl f [] xs

flatten (Elem x) = [x]
flatten (List xs) = foldl (\x y -> x ++ (flatten y)) [] xs