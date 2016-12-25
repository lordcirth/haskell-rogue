{-# Language TemplateHaskell #-} -- For Lenses

-- export everything defined here, and submodules
module GameState
    ( module GameState
    , module GameState.Board
    , module GameState.Creatures
    ) where

-- submodules
import GameState.Board
import GameState.Creatures


import qualified Data.Map as M
import Control.Lens

-- The entire game state
data GameState = GameState  { _gameBoard    :: Board
                            , _turnNum      :: Int
                            , _player       :: Player
                            , _monsters     :: [Monster]
                            , _messages     :: [String] -- Message buffer
                            }
makeLenses '' GameState
makeLenses '' CreatureInfo

initialPlayerCInfo :: CreatureInfo
initialPlayerCInfo  = CreatureInfo  { _position = (4,4)
                                    , _cDisplay = '@'
                                    , _health   = (stat 50)
                                    }

initialPlayer :: Player
initialPlayer = Player  { _pInfo = initialPlayerCInfo
                        }

initialState :: GameState
initialState = GameState    { _gameBoard    = ( boardGen (16, 16))
                            , _turnNum      = 0 -- Not yet used for anything
                            , _player       = initialPlayer
                            , _monsters     = [monster_kobold (8,8)]
                            , _messages     = []
                            }


-- Apply a function to every tile and return the new GameState
forAllTilesDo :: (Tile -> Tile) -> GameState -> GameState
forAllTilesDo func gs = over (gameBoard.tiles) (fmap func) gs
