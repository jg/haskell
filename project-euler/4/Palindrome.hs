import Data.List

isPalindrome n =
  reverse (show n) == show n

pairs lst = [(x,y) | (x:xt) <- tails lst, y <- xt]

pairProduct x = (fst x) * (snd x)

result = maximum $ filter isPalindrome (map pairProduct (pairs [100..999]))

-- threeDigitNumbers = [100..999]
