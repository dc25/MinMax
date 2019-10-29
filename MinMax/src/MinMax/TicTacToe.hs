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
{-# LANGUAGE BinaryLiterals #-}

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
import Data.Function
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

