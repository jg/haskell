{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Control.Applicative
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = sequence (replicate n die)

pr :: Rand StdGen DieValue -> IO ()
pr r =
  (putStrLn . show) =<< evalRandIO r
------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield attackers defenders) = do
  a <- dice attackers
  d <- dice defenders
  return $ updateBattleField b (fmap unDV a) (fmap unDV d)
    where
      updateBattleField :: Battlefield -> [Int] -> [Int] -> Battlefield
      updateBattleField (Battlefield ba bd) a d = Battlefield (ba + (fst (updates a d)))
                                                              (bd + (snd (updates a d)))

      -- takes lists of attacker and defenders rolls, returns update sums
      updates :: [Int] -> [Int] -> (Int, Int)
      updates a d = (sumCountUpdates (fmap rollToArmyCountUpdate (matchRolls a d)))

      matchRolls :: [Int] -> [Int] -> [(Int, Int)]
      matchRolls a d = zip (sort a) (sort d)

      sumCountUpdates :: [(Int, Int)] -> (Int, Int)
      sumCountUpdates updatePairs =
        let (xs, ys) = unzip updatePairs in
        (sum xs, sum ys)

      rollToArmyCountUpdate :: (Int, Int) -> (Int, Int)
      rollToArmyCountUpdate rollPair = if fst rollPair > snd rollPair then
                                      (0, -1)
                                   else
                                     (-1, 0)

-- simulates an entire invasion attempt, that is, repeated calls to battle until there are no defenders remaining, or fewer than two attackers.
invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield@(Battlefield atk def) =
  if (def == 0) || (atk < 2) then
    return (battlefield)
    else do
    b <- battle battlefield
    newBattlefield <- invade b
    return(newBattlefield)
