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

------------------------------------------------------------------------------
--  IArray
------------------------------------------------------------------------------

class IArray t where
  type ArrayIndex   t
  type ArrayElement t

  getElement :: ArrayIndex t -> t -> ArrayElement t

------------------------------------------------------------------------------
--  Array
------------------------------------------------------------------------------

import Data.Vector (Vector, generate)
import qualified Data.Vector as Vector

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

data Array i a = Array (Vector a)

array :: forall i a . IIndex i => (i -> a) -> Array i a
array f = Array <| generate (fromIndex (maxBound :: i)) (f << toIndex)

instance IIndex i => IArray (Array i a) where
  type ArrayIndex   (Array i a) = i
  type ArrayElement (Array i a) = a

  getElement i (Array v) = v Vector.! fromIndex i

------------------------------------------------------------------------------
--  List
------------------------------------------------------------------------------

data List a = Nil | Cons a (List a)

------------------------------------------------------------------------------
--  NonEmptyList
------------------------------------------------------------------------------

data NonEmptyList a = NonEmptyList { neHead :: a, neTail :: List a }

minimumOn :: Ord b => (a -> b) -> NonEmptyList a -> a
minimumOn f (NonEmptyList x xs) = go x xs
  where
    go m (Nil      ) = m
    go m (Cons x xs) = if f x < f m then go x xs else go m xs

maximumOn :: Ord b => (a -> b) -> NonEmptyList a -> a
maximumOn f (NonEmptyList x xs) = go x xs
  where
    go m (Nil      ) = m
    go m (Cons x xs) = if f x > f m then go x xs else go m xs
