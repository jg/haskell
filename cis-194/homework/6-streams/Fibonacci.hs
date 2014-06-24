module Fib where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a, b) -> (b, a+b)) (0,1)


data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

instance Show a => Show (Stream a) where
  show (Cons x s) =
    show x ++ ", " ++ (concat . map (\s -> show s ++ ", ") . take 20 . streamToList) s


streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

nats :: Stream Integer
nats = streamFromSeed (\n -> n + 1) 0

evens :: Stream Integer
evens = streamFromSeed (\n -> n + 2) 2

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x a) (Cons y b) =
  Cons x $ Cons y $ interleaveStreams a b

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0)
                          (interleaveStreams (streamRepeat 1) (streamFromSeed (+1) 1))

