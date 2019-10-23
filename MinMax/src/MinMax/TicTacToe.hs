------------------------------------------------------------------------------
--
--  MinMax.TicTacToe
--
------------------------------------------------------------------------------

{-# LANGUAGE TypeApplications #-}     
{-# LANGUAGE ScopedTypeVariables #-}     
{-# LANGUAGE MultiParamTypeClasses #-}     
{-# LANGUAGE DeriveGeneric #-}     
{-# LANGUAGE InstanceSigs  #-}     

module MinMax.TicTacToe
  ( test
  ) where

------------------------------------------------------------------------------
--  imports
--
--  Core instead of Prelude (see Game.hs)
------------------------------------------------------------------------------

-- import Core
import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe
import GHC.Generics
import MinMax.Data as MMDa
import MinMax.Game

------------------------------------------------------------------------------
--  TTT - Tac-Tac-Toe Board state
--
--  player - keep track of current player. (only used to print TTT correctly)
--  xbits  - positions where X has been played (stored as bits in an Int)
--  obits  - positions where O has been played
------------------------------------------------------------------------------

data TTT = TTT { player :: Player, xbits :: Int, obits :: Int } deriving (Eq, Ord)

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

data Player = X | O deriving (Eq, Ord, Show)

instance Negate Player where
  negate X = O
  negate O = X

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

data Value = Lose | Draw | Win deriving (Generic, Eq, Ord, Show)

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

ra :: Int -> Int -> [Int]
ra b e | (b == e) = []
       | otherwise = b : (ra (b+1) e)

positions :: [Int]
positions = ra 0 positionCount

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

  toIndex   xos           = TTT O (playerMask .&. xos) (playerMask .&. shiftR xos playerBits)
  fromIndex (TTT b xs os) = xs .|. shiftL os playerBits

------------------------------------------------------------------------------
--  A Tic-Tac-Toe Game
------------------------------------------------------------------------------

instance Game TTT Value where
  moves (TTT b xs os) | isWinning size xs = Left Win
                      | isWinning size os = Left Lose
                      | True              = result $ mapMaybe move positions
    where
      -- the xo and os are swapped after making a move
      -- for the AI the current player is always X

      move position = if testBit (xs .|. os) position then Nothing else Just $ TTT (MMDa.negate b) (os .|. bit position) xs

      -- if there are no moves return Draw

      result ([]       ) = Left Draw
      result (x:xs) = Right $ x:xs

------------------------------------------------------------------------------
--  isWinning - is a state winning?
--
--  This should really be a generic function, but I hand coded it to save development time
--  The hand coded version has better performance in any case
------------------------------------------------------------------------------

isWinning size = if size == 2 then isWinning2 else isWinning3

isWinning2 :: Int -> Bool
isWinning2 n | n == f2 b0 b1 = True  -- 11 00
             | n == f2 b2 b3 = True  -- 00 11
             | n == f2 b0 b2 = True  -- 10 10
             | n == f2 b1 b3 = True  -- 01 01
             | n == f2 b0 b3 = True  -- 10 01
             | n == f2 b1 b2 = True  -- 01 10
             | True          = False

isWinning3 :: Int -> Bool
isWinning3 n | n == f3 b0 b1 b2 = True -- 111 000 000
             | n == f3 b3 b4 b5 = True -- 000 111 000
             | n == f3 b6 b7 b8 = True -- 000 000 111
             | n == f3 b1 b3 b6 = True -- 100 100 100
             | n == f3 b2 b4 b7 = True -- 010 010 010
             | n == f3 b3 b5 b8 = True -- 001 001 001
             | n == f3 b0 b4 b8 = True -- 100 010 001
             | n == f3 b2 b4 b6 = True -- 001 010 100
             | True             = False

f2 b0 b1    = b0 .|. b1
f3 b0 b1 b2 = b0 .|. b1 .|. b2

b0  = (2 :: Int) ^ (0 :: Int)
b1  = (2 :: Int) ^ (1 :: Int)
b2  = (2 :: Int) ^ (2 :: Int)
b3  = (2 :: Int) ^ (3 :: Int)
b4  = (2 :: Int) ^ (4 :: Int)
b5  = (2 :: Int) ^ (5 :: Int)
b6  = (2 :: Int) ^ (6 :: Int)
b7  = (2 :: Int) ^ (7 :: Int)
b8  = (2 :: Int) ^ (8 :: Int)

------------------------------------------------------------------------------
--  turn TTT into a printable document
------------------------------------------------------------------------------

instance Show TTT where
  show (TTT O xs os) = tttDoc xs os
  show (TTT X xs os) = tttDoc os xs


tttDoc xs os = concat $ 
                 fmap (++"\n") $ 
                 intersperse divider $ 
                 fmap (concat . (intersperse "|")) $ 
                 chunksOf size $ 
                 fmap xoDoc [0..positionCount-1]
  where
    xoDoc position | testBit xs position = " X "
                   | testBit os position = " O "
                   | True                = "   "

    divider = concat $ intersperse "+" $ replicate size $ "---"

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

test :: IO ()
test = do
  print ("playerBits  = " ++ show playerBits)
  print ("playerMask  = " ++ show playerMask)
  print ("totalStates  = " ++ show totalStates)
  print ""

  play (zero @TTT)

