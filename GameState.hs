-- export everything
module GameState where

import qualified Data.Map as M


-- a square of the board
data Tile = Tile { walkable :: Bool
                 , display  :: Char
                 }

-- the game emptyBoard / grid
data Board = Board  { x     :: Int
                    , y     :: Int
                    , tiles :: M.Map (Int, Int) Tile
                    }

-- The entire game state
data GameState = GameState { gameBoard  :: Board
                           , turnNum    :: Int
                          }

floorTile   :: Tile
floorTile   = Tile True '.'

wallTile    :: Tile
wallTile    = Tile False '#'

-- return a grid of floor tiles
emptyBoard :: Int -> Int -> Board
emptyBoard sizeX sizeY = Board sizeX sizeY $
    M.fromList (zip pairs [floorTile])
    where pairs = [ (x,y) | x <- [0..sizeX], y <- [0..sizeY] ]

initialState :: GameState
initialState = GameState { gameBoard = ( emptyBoard 8 8), turnNum = 0 }
