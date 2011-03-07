module Problem1 where

myLast [x] = x
myLast (_:xs) = myLast(xs)
