------------------------------------------------------------------------------
--
--  MinMax.TicTacToe
--
------------------------------------------------------------------------------
{-# LANGUAGE BinaryLiterals #-}

module Samples.MinMax.TicTacToe
  ( test
  ) where

------------------------------------------------------------------------------
--  imports
--
--  Core instead of Prelude (see Game.hs)
------------------------------------------------------------------------------

import Core
import Data.Bits
import Data.Function
import Samples.MinMax.Game

------------------------------------------------------------------------------
--  TTT - Tac-Tac-Toe Board state
--
--  player - keep track of current player. (only used to print TTT correctly)
--  xbits  - positions where X has been played (stored as bits in an Int)
--  obits  - positions where O has been played
------------------------------------------------------------------------------

data TTT = TTT { player :: Player, xbits :: Int, obits :: Int } deriving (Eq)

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

data Player = X | O deriving (Eq)

instance Negate Player where
  negate X = O
  negate O = X

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

data Value = Lose | Draw | Win deriving (Generic, Eq, Ord, IDoc)

instance Negate Value where
  negate Lose = Win
  negate Draw = Draw
  negate Win  = Lose

------------------------------------------------------------------------------
--  !!!! TTT could be parameterized over size with dependent types
--       but instead just use the global constant 'size'
------------------------------------------------------------------------------

size          :: Int = 3
positionCount :: Int = size * size
playerBits    :: Int = positionCount
playerMask    :: Int = 2 ^ playerBits - 1
totalStates   :: Int = fromIndex (maxBound @TTT)

-- positions - a List of the possible positions
--   where
--     range min max = [min .. (max - 1)]

positions :: List Int
positions = range 0 positionCount

------------------------------------------------------------------------------
--  each state TTT has a unique Int index
--
--  class Zero - types that have a zero element
------------------------------------------------------------------------------

instance Zero    TTT where zero = TTT X 0 0
instance Bounded TTT where
  minBound = zero
  maxBound = TTT X playerMask playerMask

instance IIndex TTT where
  toIndex   :: Int -> TTT
  fromIndex :: TTT -> Int

  toIndex   xos           = TTT (if (popCount xos `mod` 2) == 1 then O else X) (playerMask .&. xos) (playerMask .&. shiftR xos playerBits)
  fromIndex (TTT b xs os) = xs .|. shiftL os playerBits

------------------------------------------------------------------------------
--  A Tic-Tac-Toe Game
------------------------------------------------------------------------------

instance Game TTT Value where
  maximizersTurn (TTT b _ _) = b == X

  moves (TTT b xs os) | isWinning size xs = Left (if b == X then Lose else Win)
                      | isWinning size os = Left (if b == X then Win else Lose)
                      | True              = result <| mapMaybe move positions
    where
      -- the xo and os are swapped after making a move
      -- for the AI the current player is always X

      move position = if testBit (xs .|. os) position then None else Some <| TTT (negate b) (os .|. bit position) xs

      -- if there are no moves return Draw

      result (Nil      ) = Left Draw
      result (Cons x xs) = Right <| NonEmptyList x xs

------------------------------------------------------------------------------
--  isWinning - is a state winning?
--
--  This should really be a generic function, but I hand coded it to save development time
--  The hand coded version has better performance in any case
------------------------------------------------------------------------------

isWinning size = if size == 2 then isWinning2 else isWinning3

testBits :: Int -> Int -> Bool
testBits bits mask = ((bits .&. mask) == mask)

isWinning2 :: Int -> Bool
isWinning2 n =    testBits n 0b0011 
               || testBits n 0b1100
               || testBits n 0b0101
               || testBits n 0b1010
               || testBits n 0b0110
               || testBits n 0b1001

isWinning3 :: Int -> Bool
isWinning3 n =    testBits n 0b000000111 
               || testBits n 0b000111000 
               || testBits n 0b111000000 
               || testBits n 0b001001001 
               || testBits n 0b010010010 
               || testBits n 0b100100100 
               || testBits n 0b100010001
               || testBits n 0b001010100 

------------------------------------------------------------------------------
--  turn TTT into a printable document
------------------------------------------------------------------------------

instance IDoc TTT where
  doc (TTT O xs os) = tttDoc xs os
  doc (TTT X xs os) = tttDoc os xs

tttDoc xs os = columnDoc <| intersperse divider <| map lineDoc <| chunk size <| fmap xoDoc positions
  where
    xoDoc position | testBit xs position = doc " X "
                   | testBit os position = doc " O "
                   | True                = doc "   "

    lineDoc :: List Doc -> Doc
    lineDoc = sepWithDoc (doc "|") << map doc

    divider = sepWithDoc (doc "+") <| replicate size <| doc "---"

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

test :: IO ()
test = do
  [printf|playerBits  = $playerBits|]
  [printf|playerMask  = $playerMask|]
  [printf|totalStates = $totalStates|]
  print ""

  play (zero @TTT)
