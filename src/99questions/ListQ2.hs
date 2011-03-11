-- Problem 11
-- (*) Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no 
-- duplicates it is simply copied into the result list. Only elements with duplicates 
-- are transferred as (N E) lists.
data Encoding a = Single a | Multiple Int a deriving (Show)
encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified [] = []
encodeModified l = let x = head l
                       (prefix, remainder) = span (==x) l
                   in
                    if (length prefix) == 1 
                    then (Single x) : (encodeModified remainder)
                    else (Multiple (length prefix) x) : (encodeModified remainder)

-- Problem 12
-- (**) Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. 
-- Construct its uncompressed version.
decodeModified :: [Encoding a] -> [a]
decodeModified [] = []
decodeModified (x:xs) =  case x of
  Single s -> s : decodeModified xs
  Multiple i m -> (replicate i m) ++ (decodeModified xs)

-- Problem 13
-- (**) Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly. 
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, 
-- but only count them. As in problem P11, simplify the result list by replacing 
-- the singleton lists (1 X) by X.
-- This particular problem kicked my ass but it helps one to  
-- better understand foldr and as patterns.
encode' :: (Eq a) => [a] -> [(Int, a)]
encode' l = foldr encodeHelper [] l

encodeHelper x [] = [(1, x)]
encodeHelper x (y@(a,b):ys)
  | x == b    = (1+a, x):ys
  | otherwise = (1,x):y:ys
  
encodeDirect :: (Eq a) => [a] -> [Encoding a]
encodeDirect = map f . encode'
  where
    f (1, x) = Single x
    f (n, x) = Multiple n x

-- Problem 14
-- (*) Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Problem 15
-- (**) Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli [] n = []
repli l n = concat $ map (replicate n) l

-- Problem 16
-- (**) Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery l n = (take (n-1) l) ++ (dropEvery (drop n l) n)

-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.


-- Problem 18
-- (**) Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements 
-- between the i'th and k'th element of the original list (both limits included). 
-- Start counting the elements with 1.

-- Problem 19
-- (**) Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate l n = let len = length l 
                 ll  = l ++ l
             in take len $ drop (if n < 0 then len + n else n) ll


-- Problem 20
-- (*) Remove the K'th element from a list.


