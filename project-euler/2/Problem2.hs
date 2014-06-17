fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

result = sum (filter even (takeWhile (<4000000) fibs))

main = do
  print $ result
