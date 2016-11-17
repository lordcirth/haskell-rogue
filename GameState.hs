{-# Language TemplateHaskell #-} -- For Lenses

-- export everything
module GameState where

import qualified Data.Map as M
import Control.Lens


--TODO: Start using Lenses!

-- Board stuff:

-- a square of the board
data Tile = Tile    { _walkable :: Bool
                    , _tDisplay  :: Char
                    }

-- the game emptyBoard / grid
data Board = Board  { _x     :: Int
                    , _y     :: Int
                    , _tiles :: M.Map (Int, Int) Tile
                    }

-- Creatures:

data CreatureInfo = CreatureInfo    { _position  :: (Int, Int)
                                    , _cDisplay  :: Char
                                    }

data Player = Player        { _cInfo     :: CreatureInfo
                            }

-- The entire game state
data GameState = GameState  { _gameBoard :: Board
                            , _turnNum   :: Int
                            , _player    :: Player
                            }

makeLenses '' Tile
makeLenses '' Board
makeLenses '' GameState
makeLenses '' Player
makeLenses '' CreatureInfo

floorTile   :: Tile
floorTile   = Tile True '.'

wallTile    :: Tile
wallTile    = Tile False '#'

-- return a grid of floor tiles
emptyBoard :: Int -> Int -> Board
emptyBoard sizeX sizeY = Board sizeX sizeY $
    M.fromList (zip pairs (repeat floorTile) )
    where pairs = [ (x,y) | x <- [1..sizeX], y <- [1..sizeY] ]

initialPlayer :: Player
initialPlayer = Player  { _cInfo =
                          CreatureInfo  { _position = (4,4)
                                        , _cDisplay = '@'
                                        }
                        }

initialState :: GameState
initialState = GameState    { _gameBoard = ( emptyBoard 16 16)
                            , _turnNum = 0
                            , _player = initialPlayer
                            }

-- shortcut functions

-- Apply a function to every tile and return the new GameState
forAllTilesDo :: (Tile -> Tile) -> GameState -> GameState
forAllTilesDo func gs = over (gameBoard.tiles) (fmap func) gs
