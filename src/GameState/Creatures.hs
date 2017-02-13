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

-- Contains all the stats, for quick > compare
data Stats = Stats  { _strength     :: Int
                    , _dexterity    :: Int
                    , _power        :: Int
                    , _control      :: Int
                    } deriving (Eq, Ord, Show)

-- Typeless is for special / irresistable dmg
data DamageType = Physical | Magical | Typeless

-- Creatures:
                                    -- General things
data Creature = Creature            { _cInfo        :: CreatureInfo

                                    -- If Player,   player-specific things
                                    , _m_player     :: Maybe Player

                                    -- If Monster,  monster-specific things
                                    , _m_monster    :: Maybe Monster

                                    } deriving Eq

data CreatureInfo = CreatureInfo    { _position     :: (Int, Int)
                                    , _cDisplay     :: Char
                                    , _health       :: Resource
                                    } deriving Eq

data Monster = Monster              { _name         :: String
                                    } deriving Eq

data Player = Player                { _stats        :: Stats
                                    } deriving Eq

makeLenses '' Resource
makeLenses '' Stats
makeLenses '' CreatureInfo
makeLenses '' Monster
makeLenses '' Player
