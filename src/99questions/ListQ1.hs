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
