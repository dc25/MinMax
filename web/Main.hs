-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BinaryLiterals #-}

-- | Haskell module declaration
module Main where

import Data.Map
import Data.Bits
import Control.Monad.State

-- | Miso framework import
import Miso
import Miso.String (MisoString, ms)
import Miso.Svg hiding (height_, style_, width_)

import MinMax.Game
import MinMax.TicTacToe as MMT

type Pos=(Int,Int)

data Cell = XMark | OMark | NoMark

type Board = Map Pos Cell

data Model = Model { game :: TTT
                   , cache :: Map TTT Value
                   } deriving Eq

-- | Sum type for application events
data Action
  = LeftPick Pos
  | NoOp
  deriving (Show, Eq)

cellSize :: Int
cellSize = 100

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp -- initial action to be executed on application load
    model  = Model (TTT X 0 0) empty            -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

xMove :: TTT -> Pos -> TTT
xMove (TTT _ xs os) (xCoord, yCoord) =
   TTT O (xs .|. bit (yCoord * MMT.size + xCoord)) os
    
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel action m@(Model game cache) =
  case action of
    NoOp
      -> noEff m
    LeftPick pos
      -> noEff (let userMove = xMove game pos
                    (ng, newCache) = runState (play userMove) cache
                in case ng of
                     Right newGame -> Model newGame newCache
                     Left v -> Model userMove newCache)

showX :: [View Action]
showX  =
  [ line_
      [ x1_ "0.8"
      , y1_ "0.8"
      , x2_ "0.2"
      , y2_ "0.2"
      ]
      []
  , line_
      [ x1_ "0.2"
      , y1_ "0.8"
      , x2_ "0.8"
      , y2_ "0.2"
      ]
      []
  ]


showO :: [View Action]
showO  =
  [ circle_
      [ cx_ "0.5"
      , cy_ "0.5"
      , r_ "0.3"
      , style_ $ fromList [("fill-opacity", "0")]
      ]
      []
  ]


showSquare :: Bool -> Pos -> Cell -> [View Action]
showSquare disableClick (xCoord, yCoord) cell =
  rect_
     ([ x_ "0.05"
      , y_ "0.05"
      , width_ "0.9"
      , height_ "0.9"
      , style_ $ fromList [("fill", "grey")]
      ]

      ++ [ onClick (LeftPick (xCoord, yCoord)) | not disableClick])

     []
  : case cell of
      XMark -> showX 
      OMark -> showO 
      NoMark -> []

showCell :: Bool -> Pos -> Cell -> View Action
showCell disableClick pos cell =
  let (x, y) = pos
      scale = show cellSize
  in g_ [ transform_
            (ms $    "scale (" ++ scale ++ ", " ++ scale ++ ") " 
                  ++ "translate (" ++ show x ++ ", " ++ show y ++ ") ")
        , strokeWidth_ ".07"
        , stroke_ "black"
        ]
        (showSquare disableClick pos cell)

cellAtPos :: TTT -> Pos -> (Pos,Cell)
cellAtPos (TTT _ xs os) pos@(xCoord, yCoord) = 
  let positionIndex = MMT.size * yCoord + xCoord
      mark | testBit xs positionIndex = XMark
           | testBit os positionIndex = OMark
           | otherwise = NoMark
  in (pos, mark) 


board :: TTT -> Map Pos Cell
board game = 
  let p = [(x, y) | x <- [0 .. MMT.size - 1], y <- [0 .. MMT.size - 1]]
  in fromList (fmap (cellAtPos game) p)

gameOver :: TTT -> Bool
gameOver (TTT _ xs os) =
  let res | isWinning xs = True
          | isWinning os = True
          | xs .|. os == 0b111111111 = True
          | otherwise = False
  in res

centerStyle :: Map MisoString MisoString
centerStyle =
  fromList [("width", "75%"), ("margin", "0 auto"), ("text-align", "center")]

viewModel :: Model -> View Action
viewModel (Model game cache) = 
  div_
    [style_ centerStyle]

    -- leading text
    [ div_ 
        [ style_ (fromList [ ("font", "italic 40px serif") ])
        ]
        [ div_ [] [text "Do you want to play a game?"]
        , div_ [] [text "(you go first...)"]
        ]

    -- the board
    , div_
        []
        [ svg_
            [ version_ "1.1"
            , width_ (ms $ show (MMT.size * cellSize))
            , height_ (ms $ show (MMT.size * cellSize))
            ]
            (let disableClick = gameOver game
                 brd = board game
                 shownBoard = mapWithKey (showCell disableClick) brd
             in fmap snd $ toList shownBoard)
        ]

    -- trailing text
    , div_ 
        [ style_ 
            (fromList [ ("text-align", "center")
                      , ("font", "italic 40px serif")
                      ])
        ]
        [ let TTT p xs os = game
              msg |  isWinning xs = "X Wins!"
                  |  isWinning os = "O Wins!"
                  |  gameOver game = "Draw"
                  |  otherwise = if p == X 
                                 then "X to move" 
                                 else "O to move"
          in div_ [] [text msg]
        ]
    ]
