{- One solution to fizzbuzz. I think it's correct
 - Took longer than 10 minutes but I'm still pretty
 - new to haskell .
 -}
fizzbuzz n
  | mod3 n && mod5 n = "FizzBuzz"
  | mod3 n = "Fizz"
  | mod5 n = "Buzz"
  | otherwise = show n 
    where mod5 x = x `mod` 5 == 0
          mod3 x = x `mod` 3 == 0

main = mapM_ (putStrLn . fizzbuzz) [1..100]
