------------------------------------------------------------------------------
--
--  MinMax.Game
--
--  generalized minmax algorithm that works for any 2 player game
--
------------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module MinMax.Game
  ( Game(..), Moves, play, Invertable(..)
  ) where

------------------------------------------------------------------------------
--  imports
------------------------------------------------------------------------------

import Data.List 
import Data.Ord
import Data.Map as DM
import Control.Monad.State

class Invertable v where
   invert :: v->v
------------------------------------------------------------------------------
--  Game
--
--  s - game state
--  v - value of a terminal game state (Lose | Draw | Win for TicTacToe)
--
--  moves - return a list of legal next states from the input state
--  maximizersTurn - True if the next player wins by maxmimzing the value of the game
------------------------------------------------------------------------------

class (Ord v, Invertable v) => Game s v | s -> v where
  moves :: s -> Moves s v

------------------------------------------------------------------------------
--  Moves
--
--  Left  v -- there are no legal moves
--  Right s -- there are    legal moves
------------------------------------------------------------------------------

type Moves s v = Either v [s]

-----------------------------------------------------------------------------
--
--  play - Given a game state return either the next move or Win/Lose/Draw
--
------------------------------------------------------------------------------

play :: (Ord s, Game s v, Invertable v) => s -> State (Map s v) (Either v s)
play s = 
  case moves s of
    Left v -> 
      return $ Left v

    Right ss -> do
      scores <- mapM score ss
      return $ Right $ fst $ maximumBy (comparing snd) scores

-----------------------------------------------------------------------------
--
--  score - 
--
------------------------------------------------------------------------------

score :: (Ord s, Game s v, Invertable v) => s -> State (Map s v) (s,v)
score s = do
  cache <- get
  case cache !? s of
    Just v -> 
      return (s, v)

    Nothing -> do
      val <- 
        case moves s of
          Left value -> return value

          Right ss -> do
            scores <- mapM score ss
            return $ invert $ snd $ maximumBy (comparing snd) scores

      c <- get
      put $ DM.insert s val c
      return (s, val)

