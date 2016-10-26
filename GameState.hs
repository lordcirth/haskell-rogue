-- export everything
module GameState where

import qualified Data.Map as M


-- a square of the board
data Tile = Tile    { walkable :: Bool
                    , tDisplay  :: Char
                    }

-- the game emptyBoard / grid
data Board = Board  { x     :: Int
                    , y     :: Int
                    , tiles :: M.Map (Int, Int) Tile
                    }

-- The entire game state
data GameState = GameState  { gameBoard :: Board
                            , turnNum   :: Int
                            , player    :: Player
                            }

data Player = Player        { cInfo     :: CreatureInfo
                            }

data CreatureInfo = CreatureInfo    { position  :: (Int, Int)
                                    , cDisplay  :: Char
                                    }

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
initialPlayer = Player  { cInfo =
                          CreatureInfo  { position = (4,4)
                                        , cDisplay = '@'
                                        }
                        }

initialState :: GameState
initialState = GameState    { gameBoard = ( emptyBoard 16 16)
                            , turnNum = 0
                            , player = initialPlayer
                            }


