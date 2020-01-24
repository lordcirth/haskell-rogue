{-# Language TemplateHaskell #-} -- For Lenses

-- Players, Monsters, Stats, etc
module GameState.Creatures where

import Data.Maybe
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
data DamageType = Physical | Magical | Typeless deriving Eq

-- Creatures:
                                    -- General things
data Creature = Creature            { _cInfo        :: CreatureInfo

                                    -- If Player,   player-specific things
                                    , _mPlayerInfo      :: Maybe PlayerInfo

                                    -- If Monster,  monster-specific things
                                    , _mMonsterInfo :: Maybe MonsterInfo

                                    } deriving Eq

data CreatureInfo = CreatureInfo    { _position     :: (Int, Int)
                                    , _cDisplay     :: Char
                                    , _health       :: Resource
                                    } deriving Eq

                                    -- What monster-specific data we have
data MonsterInfo = MonsterInfo      { _name         :: String
                                    } deriving Eq

                                    -- What player-specific data we have
data PlayerInfo = PlayerInfo        { _stats        :: Stats
                                    } deriving Eq

makeLenses '' Resource
makeLenses '' Stats
makeLenses '' Creature
makeLenses '' CreatureInfo
makeLenses '' MonsterInfo
makeLenses '' PlayerInfo

isPlayer :: Creature -> Bool
isPlayer c = isJust $ c^.mPlayerInfo

isMonster :: Creature -> Bool
isMonster c = isJust $ c^.mMonsterInfo
