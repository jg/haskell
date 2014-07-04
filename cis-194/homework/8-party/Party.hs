module Party where

import Data.Monoid
import Data.Tree

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ fun) (GL employeeList funSum) =
  GL (employeeList ++ [e])  (funSum + fun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL gl1 fun1) (GL gl2 fun2) = GL (gl1 ++ gl2) (fun1 + fun2)
  
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2) = if fun1 > fun2 then
                                      gl1
                                    else
                                      gl2

-- | Folds over a rose tree.
--
-- >>> treeFold (\x xs -> x + sum xs) Node {rootLabel = 1, subForest = [Node {rootLabel = 7, subForest = []}]}
-- 8
-- >>> treeFold (\_ xs -> 1 + length xs) Node {rootLabel = 1, subForest = [Node {rootLabel = 7, subForest = []}, Node {rootLabel = 33, subForest = []}]}
-- 3
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node label forest) = f label (map (treeFold f) forest)
