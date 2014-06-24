module JoinList where

import Data.Monoid

import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m
  
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l `mappend` tag r) l r

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ index (Append _ l r) =
  if index <= leftBranchSize then
    indexJ (index) l
  else
    indexJ (index-leftBranchSize) r
  where
    leftBranchSize = getSize (size (tag l))
indexJ index (Single _ a) = if index == 1 then
                              Just a
                            else
                              Nothing
indexJ _ Empty = Nothing                              

tree :: JoinList Size Char
tree = (Append (Size 4)
        (Append (Size 3)
         (Single (Size 1) 'y')
         (Append (Size 2)
          (Single (Size 1) 'e')
          (Single (Size 1) 'a')))
        (Single (Size 1) 'h'))

toList :: JoinList m a -> [a]
toList Empty = []
toList (Single _ a) = [a]
toList (Append _ l1 l2) = toList l1 ++ toList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 t = t
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append _ l r) =
  if n < leftBranchSize then
    (dropJ n l) +++ r
  else
    dropJ (n-leftBranchSize) r
  where
    leftBranchSize = getSize (size (tag l))

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ _ Empty = Empty
takeJ _ single@(Single _ _) = single
takeJ n t@(Append m l r)
  | n >= getSize (size m) = t
  | n <= leftBranchSize   = takeJ n l
  | otherwise             = l +++ takeJ (n - leftBranchSize) r
                            where
                              leftBranchSize = getSize (size (tag l))

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
