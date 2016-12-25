{-# Language TemplateHaskell #-} -- For Lenses

-- Players, Monsters, Stats, etc
module GameState.Creatures where

import Control.Lens
 
-- ie HP, MP
data Resource = Resource    { _cap      :: Int
                            , _current  :: Int
                            } deriving Eq

-- "smart constructor"
resource :: Int -> Resource
resource a = Resource {_cap = a, _current = a}

-- Creatures:
data CreatureInfo = CreatureInfo    { _position     :: (Int, Int)
                                    , _cDisplay     :: Char
                                    , _health       :: Resource
                                    } deriving Eq

data Monster = Monster      { _name     :: String
                            , _mInfo    :: CreatureInfo
                            } deriving Eq

data Player = Player        { _pInfo    :: CreatureInfo
                            }

makeLenses '' Resource
makeLenses '' CreatureInfo
makeLenses '' Monster
makeLenses '' Player
