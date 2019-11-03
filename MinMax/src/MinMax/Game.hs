------------------------------------------------------------------------------
--
--  MinMax.Game
--
--  generalized minmax algorithm that works for any 2 player game
--
------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module MinMax.Game
  ( Game(..), Moves, play
  ) where

------------------------------------------------------------------------------
--  imports
--
--  I use Core instead of Prelude. Core matches the Mana standard library
--  I have tried to make the abstractions in Core cleaner (subjectively of course)
--  and safer than Prelude (no partial function/etc.)
--
--  unfortunately Core is unreleased so you won't be able to build this file
--
--  some operators have been renamed:
--
--    (<<) = (.)
--    (<|) = ($)
--    (|>) = flip ($)
--
--  custom type classes:
--
--    IIndex - array index
--    Negate - negate separate from Num
--    IDoc   - documents with layout. used instead of Show.  can be constructed using nice printf syntax
--
--  custom data types:
--
--    NonEmptyList - can't be Nil
--
--      maximumOn :: Ord b => (a -> b) -> NonEmptyList a -> a
--      maximumOn over NonEmptyLists is total
--
--    Array i a - lazy array indexed by IIndex i
--
------------------------------------------------------------------------------

import Prelude(IO, Show, Either(Left,Right), Ord, ($), (.), print, return, id, Bounded(maxBound), Bool)
import Data.Maybe
import Data.List
import Data.Vector (Vector, generate)
import qualified Data.Vector as Vector
import MinMax.Data as MMDa

------------------------------------------------------------------------------
--  Game
--
--  s - game state
--  v - value of a terminal game state (Lose | Draw | Win for TicTacToe)
--
--  turns out the game abstraction only needs one member
--
--  moves - return a list of legal next states from the input state
------------------------------------------------------------------------------

class (Ord v, Negate v, IIndex s) => Game s v | s -> v where
  moves :: s -> Moves s v
  maximizersTurn :: s -> Bool

------------------------------------------------------------------------------
--  Moves
--
--  Left  v -- there are no legal moves
--  Right s -- there are    legal moves
------------------------------------------------------------------------------

type Moves s v = Either v ([s])

------------------------------------------------------------------------------
--  play - The AI plays through a game given some initial state
--
--  caveat - play stops playing as soon as it reaches a state from which it is guaranteed to win
--           this is an issue with 'play' not the algorithm used
--           this wouldn't be too hard to fix
------------------------------------------------------------------------------

play :: forall s v . (IIndex s, Show s, Game s v) => s -> IO ()
play = go
  where
    go s = do
      print s
      print ""
      case (moves s) of
        Left v   -> return ()
        Right ss -> 
          let maxMinOn = 
                if maximizersTurn s 
                then maximumOn
                else minimumOn
          in go $ maxMinOn score ss

    score :: s -> v
    score s = scores Vector.! (MMDa.fromIndex s)

    -- scores - a lazy array mapping from states to the best result that can forced from that state
    -- this needs to be local to play, but ouside of go/bestMove/score in order to get caching
    --
    -- where
    --   -- initialize an array by lazily evaluating a function at every index
    --   array :: (i -> a) -> Array i a

    --- scores :: forall s v . (IIndex s) => Vector v
    scores :: Vector v
    scores = generate (MMDa.fromIndex (maxBound :: s))
      (\s ->
        let gameState = MMDa.toIndex s
        in case moves (gameState) of
             Left  v  -> v

             Right ss -> 
               let maxMinOn = 
                     if maximizersTurn gameState
                     then maximumOn 
                     else minimumOn

               -- this one line is the minmax algorithm
               in maxMinOn id $ (map score ss) )

------------------------------------------------------------------------------
--  this algorithm is tractable for a 4x4 board
--
--  but not with lazy arrays.  There isn't enough memory
--
--  there are 2^32 states with a 4x4 board
--
--  with a lazy array each state takes a minimum of 24 bytes = 192 bits (GHC may not store things optimally)
--
--    64 bits = pointer to thunk
--    64 bits = type information in thunk
--    64 bits = Value
--
--  for a total of 96 gigabytes of memory
--
--  You could instead store the scores in a strict mutable array with 2 bits per state
--
--    00 - unevaluated
--    01 - Loss
--    10 - Draw
--    11 - Win
--
--  for a total of 1 GB memory, which would work on a typical machine
------------------------------------------------------------------------------
