This is a literate haskell program that works through chapter 1
of pearls of algoritm functional design:

The array solution of find the smallest number in a finite set X of 
natural numbers.

Need to import this for the solution. See documentation here
<a href="http://www.haskell.org/ghc/docs/6.12.2/html/libraries/array-0.3.0.0/Data-Array-IArray.html">Data.Array</a>

> import Data.Array

This is our sample set X

> x :: [Int]
> x = [1,3,0,4,6,7]

> search :: Array Int Bool -> Int
> search = length . takeWhile id . elems

> checklist :: [Int] -> Array Int Bool
> checklist xs = accumArray (||) False (0, n) 
>                (zip (filter (<= n) xs) (repeat True))
>                where n = length xs

> minfree = search . checklist

Let's break down the solution and see how neat this pearl is. 

> checklist1 :: [Int] -> [(Int, Bool)]
> checklist1 xs = let n = length xs in
>                     (zip (filter (<= n) xs) (repeat True))

checklist1 xs 
yields the following result
[(1,True),(3,True),(0,True),(4,True),(6,True)]

Most of the magic of the solution lies in what accumArray does. 

accumArray (||) False (0, n) ....

TODO: use criterion to show the performance of different solutions

