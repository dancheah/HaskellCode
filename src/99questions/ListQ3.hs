import System.Random
import Data.List
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
perm :: Int -> [a] -> [[a]]
perm n l = let len = (length l) - 1
               candidates = [ pick l i | i <- [0..len] ]
           in concat [ perm' i (n - 1) | i <- candidates ]


perm' :: (a, [a]) -> Int -> [[a]]
perm' (current, _) 0 = [ [current] ]
perm' (current, remainder) n =
  let len = (length remainder) - 1
      candidates = [ pick remainder i | i <- [0..len] ]
      newcombo   = concat [ perm' i (n - 1) | i <- candidates ]
      in [ current : i | i <- newcombo ]
-- Turns out my implementation is permutations not combinations

-- This is a super elegant solution to the question found on the wiki. I got it after
-- tracing through it for awhile. My head hurts.
combinations1 :: Int -> [a] -> [[a]]
combinations1 0 _ = [ [] ]
combinations1 n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations1 (n - 1) xs' ]

-- Another solution from the wiki. This one does the same as above but uses do notation.
combinations2 :: Int -> [a] -> [[a]]
combinations2 0 _  = return []
combinations2 n xs = do y:xs' <- tails xs
                        ys <- combinations2 (n-1) xs'
                        return (y:ys)

-- Problem 27
-- Group the elements of a set into disjoint subsets.
-- P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
combination :: Int -> [a] -> [([a], [a])]
combination 0 xs = [([], xs)]
combination _ [] = [] 
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys, zs) | (ys,zs) <- combination (n - 1) xs ]
    ds = [ (ys, x:zs) | (ys,zs) <- combination n       xs ]

-- group is also in Data.List
mygroup :: [Int] -> [a] -> [[[a]]]
mygroup [] _ = [[]]
mygroup (n:ns) xs = [ g:gs | (g, remainder) <- combination n xs, gs <- mygroup ns remainder ]

group3 :: [a] -> [[[a]]]
group3 xs = mygroup [2,3,4] xs

-- 27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]


-- Problem 28a)
-- Sorting a list of lists according to length of sublists
lsort :: [[a]] -> [[a]]
lsort l = sortBy f l 
  where
    f x y
      | (length x) > (length y) = GT 
      | (length x) < (length y) = LT
      | otherwise = EQ
                    
-- b) Again, we suppose that a list contains elements that are lists themselves. 
-- But this time the objective is to sort the elements of this list according to 
-- their length frequency
-- This is a solution. The solution is more elegant with the use of groupBy
-- found in Data.List
lfsort :: [[a]] -> [[a]]
lfsort list = lfsort1 list freqs
  where
    freqs = map head $ lsort $ group $ sort $ map length list -- sort the frequencies in the list

-- Helper function. Probably a better way to do this.
lfsort1 :: [[a]] -> [Int] -> [[a]]
lfsort1 _ [] = []
lfsort1 l (x:xs) = sublist ++ lfsort1 l xs
  where sublist = filter (\n -> length n == x) l
    
lfsort_wiki :: [[a]] -> [[a]]
lfsort_wiki lists = concat groups 
  where groups = lsort $ groupBy equalLength $ lsort lists
        equalLength xs ys = length xs == length ys
           

