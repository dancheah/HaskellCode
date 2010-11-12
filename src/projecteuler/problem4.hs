isPalindrome x = 
  let x' = show x 
      r  = reverse x'
  in x' == r

l = reverse [100..999]

calc x y = 
    let f'  = filter isPalindrome
        f'' = f' $ map (* x) y
        l   = take 1 f''
    in 
        if length l == 0
            then 0 
            else head l

-- brute force solution
a = maximum [ calc x l | x <- l ] 

mod11 x = mod x 11 == 0

-- less searching of the problem space by an order of magnitude
l' = filter mod11 l
b = maximum [ calc x l | x <- l' ] 

