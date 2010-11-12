import Data.Numbers.Primes

ub = foldr (*) 1 [1..20]
lb = foldr (*) 1 [1,2,3,5,7,11,13,17,19]

isZero a = 
  if snd a == 0 then True else False

calc n = 
  let factors = [1..20]
      x = map (mod n) factors
      x' = zip factors x
      x'' = map fst $ filter (not . isZero) x'
  in x''
     
-- Did some eyeballing of numbers
-- started multiplying by the various prime factors
-- remaining
-- solution is lb * 2 * 2 * 3
-- Did a search of the solution space which gave us the answer lb * 24
solution_set = filter (null . snd) $ zip [1..48] $ map calc $ map (* lb) [1..48]


-- problem 6 was dirt simple
problem6  = let n = [1..100]
                a = sum n ^ 2
                b = sum $ map (^ 2) n
            in a - b

-- problem 7 is also fairly simple with the haskell primes package
-- though this is worthwhile reading
-- http://www.haskell.org/haskellwiki/Prime_numbers#Prime_Number_Resources
-- I might come back to re-do these with the primes packaage
problem7 = primes !! 10000
