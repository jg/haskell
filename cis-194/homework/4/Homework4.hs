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

-- treeInsert :: Tree a -> a -> Tree a
-- treeInsert Node height l value r 

-- foldTree [] = Leaf
-- foldTree (x:xs) = Node (foldTree half0) x (foldTree half1) 
--   where (half0, half1) = splitAt (length xs `div` 2) xs

-- foldTree :: [a] -> Tree a
-- foldTree x = Node 1 (Leaf) x (Leaf)
-- foldTree (x:xs) = let Node height l@(Node lheight _ _ _) value r@(Node rheight _ _ _) = foldTree xs in 
--   if lheight > rheight then
--      Node height l value (treeInsert r x) 
--   else
--     Node height (treeInsert l x) value r

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ filter f [1..n]
  where
    nums = filter (<=n) [i+j+2*i*j|j <- [1..n], i <- [1..j]]
    f = \x -> not $ elem x nums
