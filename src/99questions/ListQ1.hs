-- Initial set of haskell questions
-- found here:
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10
module ListQ1 where

-- Problem 1
-- (*) Find the last element of a list.
myLast [x] = x
myLast (_:xs) = myLast(xs)

-- Problem 2
-- (*) Find the last but one element of a list.
myButLast [x,y]  = x
myButLast (x:xs) = myButLast(xs)

-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.
elementAt list element = list !! (element - 1)

-- Problem 4
-- (*) Find the number of elements of a list.
myLength []     = 0
myLength (x:xs) = 1 + myLength(xs)


-- Problem 5
-- (*) Reverse a list.
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse(xs) ++ [x]

-- Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can 
-- be read forward or backward; e.g. (x a m a x).
isPalindrome list = reverse list == list

-- Problem 7
-- (**) Flatten a nested list structure.
data MyList a = Elem a | List [MyList a] 
my_flatten :: MyList a -> [a]
my_flatten (Elem x) = [x]
my_flatten (List x) = concat $ map my_flatten x

-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with 
-- a single copy of the element. The order of the elements should not be changed.
compress :: (Eq a) => [a] -> [a]
compress list = compress' (tail list) (head list)
                where
                    compress' [] x' = [x']
                    compress' (x:xs) x' = if x == x' 
                                          then (compress' xs x) 
                                          else x' : (compress' xs x)

-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists.
-- Learnt this from the solution to 8 - Just use Data.List.group
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack list = 
  let x = head list
      (prefix, remainder) = span (\f -> f == x) list
  in prefix : pack remainder
                  
-- Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to 
-- implement the so-called run-length encoding data compression method. 
-- Consecutive duplicates of elements are encoded as lists (N E) where 
-- N is the number of duplicates of the element E.
-- encode1 :: (Eq a) => [a] -> [(Int, a)]
encode list = map (\x -> (length x, head x)) (pack list)
encode1 list = let packedList = pack list
               in zip (map length packedList) (map head packedList) 
