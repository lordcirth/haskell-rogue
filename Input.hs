{-# Language TemplateHaskell #-} -- For Lenses

-- export only the top-level, complete result
module Input
( handleInput
)
where


import qualified Graphics.Vty as V
import qualified Brick.Main as BMain
import qualified Brick.Types as T
import qualified Data.Map as M
import Data.Maybe   -- fromJust
import Data.List    -- delete, find


-- external libraries:
import Control.Lens

-- My own files:
import GameState

--  The structure that all Actions must return
data ActionResult = ActionResult    {
                                    -- Has the player spent his turn? eg an invalid action doesn't count
                                    -- Also enables 'free actions' before your turn, in future
                                      _costsTurn    :: Bool

                                    -- Resulting gameState messages are merged into gamestate by the actions
                                    , _gameState    :: GameState
                                    }

makeLenses '' ActionResult


-- Any action the player can attempt - just an alias
type Action = (GameState -> ActionResult)


handleInput :: GameState -> V.Event -> T.EventM (T.Next (GameState))
handleInput gs ev =
    case ev of
        -- the empty list [] is the list of mod keys
        V.EvKey V.KEsc []   -> BMain.halt gs    -- press ESC to quit

        -- use numpad keys and match on number range to get movement
        V.EvKey (V.KChar k) [] | k `elem` ['0'..'9'] -> BMain.continue (fullGameTurn (handleMoveInput k) gs)

        -- Temporary testing: when player presses 'a' attempt to attack the first monster in list
        --V.EvKey (V.KChar 'a') [] -> BMain.continue (fullGameTurn (action_melee 0) gs)

        -- Or arrow key movement
        V.EvKey V.KUp       []  -> BMain.continue (fullGameTurn (action_move ( 0,-1) ) gs)
        V.EvKey V.KDown     []  -> BMain.continue (fullGameTurn (action_move ( 0, 1) ) gs)
        V.EvKey V.KLeft     []  -> BMain.continue (fullGameTurn (action_move (-1, 0) ) gs)
        V.EvKey V.KRight    []  -> BMain.continue (fullGameTurn (action_move ( 1, 0) ) gs)

        -- for a key which does nothing, do nothing (redraw identical)
        -- could optionally print "That key does nothing!" or something?
        _                   -> BMain.continue gs


-- The player has requested to move in some direction.  Return an Action function which attempts the specific move
handleMoveInput :: Char -> (Action)
handleMoveInput k =
    -- TODO: 5 = wait or something?
    case k of
        '1' -> action_move (-1, 1)
        '2' -> action_move ( 0, 1)
        '3' -> action_move ( 1, 1)

        '4' -> action_move (-1, 0)
        '5' -> action_move ( 0, 0) -- 5 does nothing, wastes turn
        '6' -> action_move ( 1, 0)

        '7' -> action_move (-1,-1)
        '8' -> action_move ( 0,-1)
        '9' -> action_move ( 1,-1)
        _   -> error "non-numeric input to handleMoveInput"


-- run a full game turn, ie pre-turn, player turn, enemy turn, post-turn.
-- Currently, only the player exists.
fullGameTurn :: (Action) -> GameState -> GameState
fullGameTurn action gs

        -- Player hasn't spent turn - free action or invalid
        | not (result^.costsTurn)   = result^.gameState

        -- Player has spent turn - normal action completed
        -- TODO: this is where to run enemy turns, etc
        | otherwise                 = incrementTurn(result^.gameState)

        where result = playerTurn action gs

incrementTurn :: GameState -> GameState
incrementTurn gs =
    over (turnNum) (+1) gs

-- Arguments:
-- current gameState,
-- an action (function) which a player character attempts to do, ie move, or attack
-- Note that this means the player can only do one thing/keypress per turn, an acceptable limitation
--      Actually, since I have 'costsTurn' now, I have support for 'free actions' before a full action!
playerTurn :: (Action) -> GameState -> ActionResult
playerTurn  action gs =
    -- for the moment, we just do the action.
     let result = action gs in
      --  (result^.gameState)
     result

-- Return the Just Monster at the given location, or Nothing if there isn't one
monsterAt :: GameState -> (Int, Int) -> Maybe Monster
monsterAt gs pos =
    find (\m -> (m^.mInfo.position) == pos) (gs^.monsters)

-- Takes current gameState, and direction to move ie (0,1)
-- Maybe we moved, or maybe we print "you can't move there!", etc
-- GameState should be the last argument of every Action, to make it easier to partially apply!
--      Or just use the Action alias
action_move :: (Int, Int) -> (Action)
action_move move gs

    -- Check if that's actually a valid move, ie not into an enemy or wall
    -- If there's a monster there, melee it!
    | isJust $ monsterAt gs attempt  = action_melee (fromJust $ monsterAt gs attempt) gs
    | targetTile^.walkable  = result_success
    | otherwise             = result_fail

    -- Message string, did it cost the turn, new gamestate
    -- Moving will not have a message later, just for testing.
    --ActionResult  True (addMessage "player moved" resulting_gs)
    -- Return new gameState with message added
    where
        playerPos       = player.pInfo.position      -- A lens, ie gs^.playerPos
        resulting_gs    = over (playerPos) (addPos move) gs :: GameState
        attempt         = addPos (gs^.player.pInfo.position) move   :: (Int, Int)
        
        -- TODO: Safer checks than fromJust?
        targetTile      = fromJust $ M.lookup (attempt) (gs^.gameBoard.tiles)

        -- No message for moving, too spammy
        result_success  = ActionResult True  (resulting_gs)
        -- Return unchanged gs + message
        result_fail     = ActionResult False (addMessage "That's a wall!" gs)


-- Player (attempts to) attack the specified monster (by list index)
action_melee :: Monster -> (Action)
action_melee target gs

    | inMeleeRange (gs^.player.pInfo.position) (target^.mInfo.position) = result_success
    | otherwise = result_fail
    where
        -- Note that we add the message first, then damage, so as to come before the death message, etc
        result_success  = ActionResult True  (damage_monster (addMessage "attacked!" gs) target Physical 1)
        result_fail     = ActionResult False ( addMessage "Out of range!" gs)

inMeleeRange :: (Int, Int) -> (Int, Int) -> Bool
inMeleeRange one two =
    (abs (fst diff) <= 1) && (abs (snd diff) <= 1)
    where
        diff = one `subtractPos` two

replaceMonster :: GameState -> Monster -> Monster -> GameState
replaceMonster gs old new = over (monsters) (map (\i -> if i == old then new else i)) (gs)

-- gs, MonsterIndex, damage
damage_monster :: GameState -> Monster -> DamageType -> Int -> GameState
damage_monster gs target dmgType dmg

    -- If monster is dead now, delete from list instead of changing it
    | newMonster^.mInfo.health.current <= 0 = over (monsters) (delete (target) ) (addMessage kill_message  gs)

    -- replace monster with updated monster
    | otherwise = replaceMonster gs target newMonster

    where
        newMonster = over (mInfo) (damage Physical 1) (target)
        -- Debugging message:    
        --message = "Monster was at: " ++ (show $ oldMonster^.mInfo.health.current) ++ "and is now at: " ++ (show $ newMonster^.mInfo.health.current) :: String
        kill_message = "You kill the " ++ (target^.name) ++ "!"

-- Universal damage function: will calculate armor, etc
-- Because it operates on CreatureInfo's, it works for both players and monsters.
-- Try to keep the complexity in here
damage  :: DamageType -> Int -> CreatureInfo -> CreatureInfo
damage dmgType dmg oldCreature = 
    -- TODO: Armor, etc
    newCreature
    where
        newCreature = over (health.current) (subtract dmg) (oldCreature)

-- Append a message to the gamestate's buffer
-- TODO: Drop old messages once it gets too long
addMessage :: String -> GameState -> GameState
addMessage message gs
    -- TODO: Fix hardcoded buffer length
    | length (gs^.messages) > 12    = over (messages) (drop 1) (added)
    | otherwise                     = added
    where
        added   = over (messages) ( ++ [message]) (gs) :: GameState
