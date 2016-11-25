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
data Board = Board  { _size_x     :: Int
                    , _size_y     :: Int
                    , _tiles :: M.Map (Int, Int) Tile
                    }

-- Creatures:

data CreatureInfo = CreatureInfo    { _position  :: (Int, Int)
                                    , _cDisplay  :: Char
                                    }

data Player = Player        { _cInfo     :: CreatureInfo
                            }

-- The entire game state
data GameState = GameState  { _gameBoard    :: Board
                            , _turnNum      :: Int
                            , _player       :: Player
                            , _messages     :: [String] -- Message buffer
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
-- Note that the order we generate it here is irrelevant, if it's a square.
-- But, as defined in Draw.hs, X is horizontal and Y vertical, with (1,1) being top-left
emptyBoard :: (Int, Int) -> Board
emptyBoard (sizeX, sizeY) = Board sizeX sizeY $
    M.fromList (zip pairs (repeat floorTile) )
    where pairs = [ (x,y) | x <- [1..sizeX], y <- [1..sizeY] ]

boardGen :: (Int, Int) -> Board
boardGen (sizeX, sizeY) =
    over (tiles) (M.insert (1,1) wallTile) (rawBoard)

    where
        rawBoard = ( emptyBoard (sizeX, sizeY) )


initialPlayer :: Player
initialPlayer = Player  { _cInfo =
                          CreatureInfo  { _position = (4,4)
                                        , _cDisplay = '@'
                                        }
                        }

initialState :: GameState
initialState = GameState    { _gameBoard    = ( boardGen (16, 16))
                            , _turnNum      = 0 -- Not yet used for anything
                            , _player       = initialPlayer
                            , _messages    = []
                            }

-- helper functions

-- add two 2d positions
addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (x1,y1) (x2,y2) = (x1+x2, y1+y2)


-- Apply a function to every tile and return the new GameState
forAllTilesDo :: (Tile -> Tile) -> GameState -> GameState
forAllTilesDo func gs = over (gameBoard.tiles) (fmap func) gs
