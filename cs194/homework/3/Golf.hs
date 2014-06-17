module Golf where

skips :: [a] -> [[a]]
skips lst = map (\n -> everyNth n lst) [1..length lst]

everyNth :: Int -> [a] -> [a]
everyNth n lst = concatMap (\i -> take 1 (drop i lst)) [n*x-1 | x <- [1..div (length lst) n]]


localMaxima :: [Integer] -> [Integer]
localMaxima [_,_] = []
localMaxima (x:y:z:lst) = if y > x && y > z then
                             y : localMaxima (y:z:lst)
                          else
                            localMaxima (y:z:lst)

histogram :: [Integer] -> String
histogram lst =
  unlines (dotLines ++ [sep] ++ [snums])
  where
    nums = [0..9]
    snums = joins $ map show nums
    dotLines = reverse $ map dotLine [1..maximum $ map frequency nums]
    dotLine = \f -> joins $ map (dotIfhasFrequencyP f) nums
    sep  = joins $ map (\_ -> "=") nums
    joins = foldl1 (++)
    frequency = \num -> length (filter (==num) lst)
    dotIfhasFrequencyP f num = if f <= frequency num then
                                 "*"
                               else
                                 " "
