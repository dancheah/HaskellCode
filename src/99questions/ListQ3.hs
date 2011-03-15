-- Third set of 99 haskell problems found here
-- http://www.haskell.org/haskellwiki/99_questions/21_to_28

-- Problem 21
-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt value [] _ = value : []
insertAt value list 1 = value : list
insertAt value (x:xs) index = x : insertAt value xs (index - 1)

-- Problem 22
-- Create a list containing all integers within a given range.
-- range :: Int -> Int
