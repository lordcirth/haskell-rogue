-- export everything
module GameState where

import qualified Data.Map as M


-- a square of the board
data Tile = Tile { walkable :: Bool
                 , display  :: Char
                 }

-- the game emptyBoard / grid
data Board = Board  { tiles :: M.Map (Int, Int) Tile
                    }

-- The entire game state
data GameState = GameState { gameBoard  :: Board
                           , turnNum    :: Int
                          }

floorTile   :: Tile
floorTile   = Tile True '.'

wallTile    :: Tile
wallTile    = Tile False '#'

-- return a 8x8 of floor tiles
emptyBoard :: Board
emptyBoard = Board $
    M.fromList (zip pairs [floorTile])
    where pairs = [ (x,y) | x <- [0..8], y <- [0..8] ]

initialState :: GameState
initialState = GameState { gameBoard = emptyBoard, turnNum = 0 }
