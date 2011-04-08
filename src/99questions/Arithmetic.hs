-- Arithmetic related problem set found here
-- http://www.haskell.org/haskellwiki/99_questions/31_to_41

-- Problem 31
-- One easy solution is to use the primes package.
isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = let max = ceiling $ sqrt $ fromIntegral n
                divisors = filter (\x -> n `mod` x == 0) [2..max]
            in null divisors

-- Problem 32
-- Mostly correct. Doesn't handle negative numbers correctly. 
-- Could be more succint
myGCD :: Int -> Int -> Int
myGCD x y 
  | x > y     = gcd x y 
  | x < y     = myGCD y x
  | otherwise = x
  where gcd x y = let quot = x `div` y
                      rem  = x `mod` y
                  in if rem == 0 then y 
                     else myGCD y rem
            
