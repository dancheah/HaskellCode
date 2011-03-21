import System.Random
import Data.List (tails)
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
range :: Int -> Int -> [Int]
range start end = [start .. end]

-- Problem 23
-- Extract a given number of randomly selected elements from a list.
-- Example in Haskell:
-- Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
-- eda
rnd_select :: [a] -> Int -> IO [a]
rnd_select _ 0 = return []
rnd_select l n = do rnd <- randomRIO (0, len)
                    let (sel, rem) = pick l rnd in
                        do newl <- rnd_select rem (n - 1)
                           return $ sel : newl
            where len = (length l) - 1 

pick :: [a] -> Int -> (a, [a])
pick l n = (l !! n, take n l ++ drop (n + 1) l)

-- This is a solution on the wiki. The way it's doing 
-- probablities is more sophisticated. I need to convince
-- myself that it is doing the right thing
rnd_select1 :: [a] -> Int -> IO [a]
rnd_select1 _ 0 = return []
rnd_select1 (x:xs) n =
    do r <- randomRIO (0, (length xs))
       if r < n
           then do
               rest <- rnd_select1 xs (n-1)
               return (x : rest)
           else rnd_select1 xs n

-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.
-- Prelude System.Random>diff_select 6 49
-- Prelude System.Random>[23,1,17,33,21,37]
diff_select :: Int -> Int -> IO [Int]
diff_select i n = rnd_select [1..n] i

-- Problem 25
-- Generate a random permutation of the elements of a list.
-- Prelude>rnd_permu "abcdef"
-- Prelude>"badcef"
rnd_permu :: [a] -> IO [a]
rnd_permu [] = return []
rnd_permu l@(x:xs) = do rnd <- randomRIO (0, length xs)
                        let (sel, rem) = pick l rnd in
                          do newl <- rnd_permu rem
                             return $ sel : newl



-- Problem 26
-- Generate the combinations of K distinct objects chosen from the N elements of a list
combinations :: Int -> [a] -> [[a]]
combinations n l =   let len = (length l) - 1
                         candidates = [ pick l i | i <- [0..len] ]
                     in concat [ combinations' i (n - 1) | i <- candidates ]


combinations' :: (a, [a]) -> Int -> [[a]]
combinations' (current, _) 0 = [ [current] ]
combinations' (current, remainder) n =
  let len = (length remainder) - 1
      candidates = [ pick remainder i | i <- [0..len] ]
      newcombo   = concat [ combinations' i (n - 1) | i <- candidates ]
      in [ current : i | i <- newcombo ]
-- Turns out my implementation is permutations not combinations


combinations1 :: Int -> [a] -> [[a]]
combinations1 0 _ = [ [] ]
combinations1 n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations1 (n - 1) xs' ]



-- Problem 27
-- Group the elements of a set into disjoint subsets.

-- Problem 28
-- Sorting a list of lists according to length of sublists
