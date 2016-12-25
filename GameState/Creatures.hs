{-# Language TemplateHaskell #-} -- For Lenses

-- Players, Monsters, Stats, etc
module GameState.Creatures where

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

makeLenses '' Monster
makeLenses '' Player
makeLenses '' Stat
