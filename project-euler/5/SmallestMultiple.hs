isDivisible n k =
  rem n k == 0

isDivisibleByList n lst = all (isDivisible n) lst
  
result = take 1 (filter (\n -> isDivisibleByList n [1..20]) [1..])
