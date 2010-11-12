-- ifelse :: Bool -> a -> b

ifelse True e1 _ = e1
ifelse False _ e2 = e2
  
