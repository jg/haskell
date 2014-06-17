import Data.List

factors n =
  tuple_to_list(unzip [ (j, coFactor j) | j <- factorsInRange])
  where
    range = [1..truncate (sqrt (fromIntegral n))]
    factorsInRange = [i | i <- range, (mod n i) == 0]
    coFactor = div n
    tuple_to_list lt = (fst lt) ++ (snd lt)

isPrime n =
  length factorsInRange == 1
  where
    factorsInRange = [i | i <- range, (mod n i) == 0]
    range = [1..truncate (sqrt (fromIntegral n))]

primeFactors n = filter isPrime (factors n)

result = maximum $ Main.primeFactors 600851475143

