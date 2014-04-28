import Text.ParserCombinators.Parsec

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let
  smaller = quicksort (filter (<=x) xs)
  bigger = quicksort (filter (>x) xs)
  in smaller ++ [x] ++ bigger


largestDivisible = head (filter p [100000, 99999..])
  where p x = mod x 3829 == 0                   

sumOddSquared =
  sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

sumOddSquared2 =
  sum (takeWhile (<10000) [x^2 | x <- [1..], odd (x^2)])



chain :: Integral a => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (div n 2)
  | odd n  = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong  (map chain [1..100]))
  where isLong lst = length lst > 15

sum' = foldl (\acc x -> acc + x) 0


run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x       
       
testOr2 =   try (string "(a)")
        <|> string "(b)"

parens  :: Parser ()
parens  = do { char '('
            ; parens
            ; char ')'
            ; parens
            }
        <|> return ()
