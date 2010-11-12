fibs = 1 : 2 : (zipWith (+) fibs (tail fibs))

isEven n = n `mod` 2 == 0
lessThan4Million n = n <= 4000000

list = takeWhile lessThan4Million [ x | x <- fibs, isEven x ]
answer = sum list
