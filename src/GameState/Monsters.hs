module GameState.Monsters where

-- Stats, etc
import GameState.Creatures
import Control.Lens

-- Specific Monsters:
cInfoKobold :: CreatureInfo
                                -- position overwritten by monsterConstructor
cInfoKobold = CreatureInfo     { _position = (0,0)
                                , _cDisplay = 'g'
                                , _health   = resource 10
                                }
mInfoKobold ::  MonsterInfo
mInfoKobold =   MonsterInfo { _name = "kobold" }

monsterKobold     :: (Int, Int) -> Creature
monsterKobold pos = monsterConstructor "kobold" pos cInfoKobold

monsterConstructor :: String -> (Int, Int) -> CreatureInfo -> Creature
monsterConstructor monsterName pos monsterCreatureInfo =
    Creature    { _cInfo        = placedCreatureInfo
                , _mMonsterInfo = Just (MonsterInfo monsterName)
                , _mPlayerInfo  = Nothing
                }
    where
        placedCreatureInfo = set position pos monsterCreatureInfo
