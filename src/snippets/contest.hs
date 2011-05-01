-- A programming contest given to high school students that I judged. Worked
-- through these in Haskell since thats my level of Haskell programming.
import Data.List.Split
import Data.List
-- Problem 1
age_difference x y = length $ takeWhile diff $ zip [x..] [y..] 
                     where diff (a1, a2) = ((a2 - a1) / a1) > 0.15

-- Problem 2
day_of_year :: Int -> Int -> Int
day_of_year m d = month m + d
  where month x = sum $ take x [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30]
        
calc_cosmic_day :: Int -> Int -> (Int, Int)
calc_cosmic_day m d = let dy   = day_of_year m d - 1
                          mins = floor $ (1440 / 365) * (fromIntegral dy)
                          (hour, min) = mins `divMod` 60
                      in (hour, min)
                         
format :: Int -> String
format n
  | n < 10    = "0" ++ (show n)
  | otherwise = show n
                
cosmic_day s = let xs = map read $ splitOn "/" s :: [Int]
                   (hour, min) = calc_cosmic_day (xs !! 0) (xs !! 1)
                   in (format hour) ++ ":" ++ (format min)

-- Problem 3. Skipping this one. Really easy.

-- Problem 4.
spaces 0 = ""
spaces x = concat $ replicate 1 " "

line pre mid = (spaces pre) ++ "*" ++ (spaces mid) ++ "*\n"

-- Problem 5
data (Integral a) => BitShift a = RS a | LS a
                                deriving (Show)

bitshift (RS n) s = let l = length s
                    in (concat $ replicate n "0") ++ (take (l - n) s)
bitshift (LS n) s = let l = length s
                    in (drop n s) ++ (concat $ replicate n "0")


-- Problem 11
anagrams = permutations 

-- Problem 12
jumpyear n = (n `mod` 2 == 1) && (n `mod` 11 == 0)
