
import Data.Bits
import Data.List
import Data.Map as DM
import Data.List.Split
import Control.Monad.State

import MinMax.Game
import MinMax.TicTacToe as MMT

------------------------------------------------------------------------------
--  turn TTT into a printable document
------------------------------------------------------------------------------
--
instance Show TTT where
  show (TTT p  xs os) = 
    unlines 
      (intersperse divider $ 
       fmap (intercalate "|") $ 
       chunksOf MMT.size $ 
       fmap xoDoc [0..positionCount-1]) 
       ++ "Next Player: " ++ show p 
    where
      xoDoc position | testBit xs position = " X "
                     | testBit os position = " O "
                     | otherwise           = "   "
  
      divider = intercalate "+" $ replicate MMT.size "---"

autoPlay :: TTT -> Map TTT Value -> IO ()
autoPlay gameState cache = do
    print ("cache size = " ++ show (DM.size cache))
    print gameState
    putStrLn ""
    let (ns, nc) = runState (play gameState) cache
    case ns of
       Left v -> putStrLn $ "GAME OVER: " ++ show v
       Right gs -> autoPlay gs nc

main :: IO ()
main = do
  putStrLn ("playerBits  = " ++ show playerBits)
  putStrLn ("playerMask  = " ++ show playerMask)
  putStrLn ""

  autoPlay (TTT X 0 0) empty
