module MinMax.Data where

class Negate a where
    negate :: a->a
------------------------------------------------------------------------------
--  Zero
------------------------------------------------------------------------------

class Zero t where zero :: t

------------------------------------------------------------------------------
--  IIndex
------------------------------------------------------------------------------

class (Bounded i, Zero i) => IIndex i where
  fromIndex :: i -> Int
  toIndex   :: Int -> i

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f (x:xs) = go x xs
  where
    go m (y:ys) = if f y < f m then go y ys else go m ys
    go m [] = m

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f (x:xs) = go x xs
  where
    go m (y:ys) = if f y > f m then go y ys else go m ys
    go m [] = m
