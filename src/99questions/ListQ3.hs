import System.Random
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
