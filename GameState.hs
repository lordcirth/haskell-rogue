{-# Language TemplateHaskell #-} -- For Lenses

-- export everything
module GameState
    ( module GameState
    , module GameState.Board
    ) where

--reexport submodules
import GameState.Board


import qualified Data.Map as M
import Control.Lens

-- ie HP, MP
data Stat = Stat    { _cap      :: Int
                    , _current  :: Int
                    }

-- "smart constructor"
stat :: Int -> Stat
stat a = Stat {_cap = a, _current = a}

-- Creatures:
data CreatureInfo = CreatureInfo    { _position     :: (Int, Int)
                                    , _cDisplay     :: Char
                                    , _health       :: Stat
                                    }

data Monster = Monster      { _name     :: String
                            , _mInfo    :: CreatureInfo
                            }


data Player = Player        { _pInfo    :: CreatureInfo
                            }

-- The entire game state
data GameState = GameState  { _gameBoard    :: Board
                            , _turnNum      :: Int
                            , _player       :: Player
                            , _monsters     :: [Monster]
                            , _messages     :: [String] -- Message buffer
                            }
makeLenses '' GameState
makeLenses '' Monster
makeLenses '' Player
makeLenses '' Stat
makeLenses '' CreatureInfo

-- TODO: Refactor Monsters into their own file once they grow
-- Instances of Monster:
cInfo_kobold :: CreatureInfo
cInfo_kobold  = CreatureInfo    { _position = (33,33)
                                , _cDisplay = 'g'
                                , _health   = (stat 10)
                                }

monster_kobold     :: (Int, Int) -> Monster
monster_kobold pos = Monster "kobold" (CreatureInfo pos 'k' (stat 10) )


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
