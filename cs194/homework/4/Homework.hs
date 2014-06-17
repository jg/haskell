module Homework4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl (\acc x -> acc * (x-2)) 1 . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)


fun2' :: Integer -> Integer
fun2' = foldl1 (+) . filter even . takeWhile (/=1) . iterate collatz

collatz :: Integer -> Integer
collatz 1 = 1
collatz n | even n = n `div` 2
          | otherwise = 3 * n + 1


-------------------------------------------------------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree x = Node 1 (Leaf) x (Leaf)
