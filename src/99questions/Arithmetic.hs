-- Arithmetic related problem set found here
-- http://www.haskell.org/haskellwiki/99_questions/31_to_41

-- One easy solution is to use the primes package.
isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = let max = ceiling $ sqrt $ fromIntegral n
                divisors = filter (\x -> n `mod` x == 0) [2..max]
            in null divisors
