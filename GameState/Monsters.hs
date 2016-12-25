{-# Language TemplateHaskell #-} -- For Lenses

module GameState.Monsters where

-- Stats, etc
import GameState.Creatures

-- Specific Monsters:
cInfo_kobold :: CreatureInfo
cInfo_kobold  = CreatureInfo    { _position = (33,33)
                                , _cDisplay = 'g'
                                , _health   = (resource 10)
                                }

monster_kobold     :: (Int, Int) -> Monster
monster_kobold pos = Monster "kobold" (CreatureInfo pos 'k' (resource 10) )
