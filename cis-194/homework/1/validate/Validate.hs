toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = []
              | n < 10 = [n]
              | otherwise = rem n 10 : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

everyNth :: Integer -> [a] -> [a]
everyNth n = map snd . filter ((== 1) . fst) . zip (cycle [1..n])

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys             

mergeRight a b = reverse $ merge (reverse a) (reverse b)

everyNthFromRight :: Integer -> [a] -> [a]
everyNthFromRight n lst = reverse $ everyNth n (reverse lst)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = mergeRight a (map (*2) b)
  where
   a = everyNthFromRight 2 (lst)
   b = everyNthFromRight 2 (init lst)


sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits 

validate :: Integer -> Bool
validate n = (rem (sumDigits (doubleEveryOther (toDigits n))) 10) == 0
   
