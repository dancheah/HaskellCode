-- Arithmetic related problem set found here
-- http://www.haskell.org/haskellwiki/99_questions/31_to_41
import Data.List

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
            
-- Problem 33
-- Using the prelude gcd to calculate coprime
coprime x y = gcd x y == 1

-- Problem 34
-- Calculate Euler's totient function phi(m).
-- totient :: Int -> Int
totient n = length [ x | x <- [1..n-1], coprime x n]

-- Problem 35
-- Determine the prime factors of a given positive integer. 
-- Construct a flat list containing the prime factors in ascending order.
-- Didn't really get this one. The solution comes from the wiki. This
-- one appears to take advantage of corecursion and laziness.
primeFactors n = factor n primes
  where factor n (p:ps) | p*p > n = [n]
                        | n `mod` p /= 0 = factor n ps
                        | otherwise = p : factor (n `div` p) (p:ps)
        primes = 2 : filter ((==1) . length . primeFactors) [3,5..]


-- 7 Problem 36
-- Determine the prime factors of a given positive integer. This one was 
-- pretty easy once you have the prime factors function above.
prime_factors_mult = map (\x -> (head x, length x)) . group . sort . primeFactors
