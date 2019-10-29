------------------------------------------------------------------------------
--
--  MinMax.TicTacToe
--
------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}     
{-# LANGUAGE MultiParamTypeClasses #-}     
{-# LANGUAGE BinaryLiterals #-}

module MinMax.TicTacToe where

------------------------------------------------------------------------------
--  imports
--
------------------------------------------------------------------------------

import Data.Bits
import Data.Maybe
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

otherPlayer :: Player->Player
otherPlayer X = O
otherPlayer O = X

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

data Value = Lose | Draw | Win deriving (Eq, Ord, Show)

instance Invertable Value where
  invert Lose = Win
  invert Draw = Draw
  invert Win  = Lose

------------------------------------------------------------------------------
--  !!!! TTT could be parameterized over size with dependent types
--       but instead just use the global constant 'size'
------------------------------------------------------------------------------

size          = 3
size          :: Int

positionCount :: Int
positionCount = size * size

playerBits    :: Int
playerBits    = positionCount

playerMask    :: Int 
playerMask    = 2 ^ playerBits - 1

-- positions - a List of the possible positions
--   where
--     range min max = [min .. (max - 1)]

range :: Int -> Int -> [Int]
range begin end | begin == end = []
                | otherwise = begin : range (begin+1) end

positions :: [Int]
positions = range 0 positionCount

------------------------------------------------------------------------------
--  A Tic-Tac-Toe Game
------------------------------------------------------------------------------

instance Game TTT Value where
  moves (TTT b xs os) | isWinning xs = Left Win --- x wins
                      | isWinning os = Left Win --- o wins
                      | otherwise         = result $ mapMaybe move positions
    where
      move position = 
        if testBit (xs .|. os) position 
        then Nothing 
        else
          let newXs = if b == X then xs .|. bit position else xs
              newOs = if b == O then os .|. bit position else os
          in Just $ TTT (otherPlayer b) newXs newOs

      -- if there are no moves return Draw

      result [] = Left Draw
      result xss = Right xss

------------------------------------------------------------------------------
--  isWinning - is a state winning?
--
--  This should really be a generic function, but I hand coded it to save development time
--  The hand coded version has better performance in any case
------------------------------------------------------------------------------

testBits :: Int -> Int -> Bool
testBits bits mask = (bits .&. mask) == mask

isWinning :: Int -> Bool
isWinning n =     testBits n 0b000000111 
               || testBits n 0b000111000 
               || testBits n 0b111000000 
               || testBits n 0b001001001 
               || testBits n 0b010010010 
               || testBits n 0b100100100 
               || testBits n 0b100010001
               || testBits n 0b001010100 

