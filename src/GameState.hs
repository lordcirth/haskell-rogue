{-# Language TemplateHaskell #-} -- For Lenses

-- export everything defined here, and submodules
module GameState
    ( module GameState
    , module GameState.Board
    , module GameState.Creatures
    , module GameState.Monsters
    ) where

-- submodules
import GameState.Board
import GameState.Creatures
import GameState.Monsters


-- import qualified Data.Map as M
import Control.Lens
import System.Random

-- The entire game state
data GameState = GameState  { _gameBoard    :: Board
                            , _turnNum      :: Int
                            , _creatures    :: [Creature]
                            , _messages     :: [String] -- Message buffer
                            , _rng          :: StdGen
                            }
makeLenses '' GameState

initialPlayerCInfo :: CreatureInfo
initialPlayerCInfo  = CreatureInfo  { _position = (4,4)
                                    , _cDisplay = '@'
                                    , _health   = resource 50
                                    }

initialPlayerStats :: Stats
initialPlayerStats = Stats  { _strength     = 1
                            , _dexterity    = 1
                            , _power        = 1
                            , _control      = 1
                            }



initialPlayer :: Creature
initialPlayer =  Creature   { _cInfo            = initialPlayerCInfo
                            , _mPlayerInfo      = Just PlayerInfo {_stats = initialPlayerStats}
                            , _mMonsterInfo     = Nothing
                            }

-- The initialState is constant, except for initializing the RNG
initialState :: StdGen -> GameState
initialState initialRng = GameState    { _gameBoard    = boardGen (16, 16)
                                , _turnNum      = 0 -- Not yet used for anything
                                , _creatures    = [initialPlayer, monsterKobold (8,8)]
                                , _messages     = []
                                , _rng          = initialRng
                                }


-- Apply a function to every tile and return the new GameState
forAllTilesDo :: (Tile -> Tile) -> (GameState -> GameState)
forAllTilesDo func = over (gameBoard.tiles) (fmap func)

-- A lens to access 'player' quickly

--playerlens = lens getter setter
--
--    where
--        getter gs   = gs^.creatures.unsafeSingular(ix 0)
--        setter p gs = set (creatures.unsafeSingular(ix 0)) gs p

-- A handy lens
player :: Traversal' GameState Creature
player = creatures . ix 0
