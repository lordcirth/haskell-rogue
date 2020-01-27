{-# Language TemplateHaskell #-} -- For Lenses

-- export only the top-level, complete result
module Input
( handleInput
)
where


-- std libs:
import qualified Data.Map as M
import Data.Maybe   -- fromJust
import Data.List    -- delete, find
-- import System.Random


-- external libraries:
import Control.Lens
import qualified Graphics.Vty as V
import qualified Brick.Main as BMain
import qualified Brick.Types as T

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

-- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#apphandleevent-handling-events
handleInput :: GameState -> T.BrickEvent () e -> T.EventM () (T.Next GameState)
handleInput gs (T.VtyEvent ev) =
    case ev of
        -- the empty list [] is the list of mod keys
        V.EvKey V.KEsc []   -> BMain.halt gs    -- press ESC to quit

        -- use numpad keys and match on number range to get movement
        V.EvKey (V.KChar k) [] | k `elem` ['0'..'9'] -> BMain.continue (fullGameTurn (handleMoveInput k) gs)

        -- Temporary testing: when player presses 'a' attempt to attack the first monster in list
        --V.EvKey (V.KChar 'a') [] -> BMain.continue (fullGameTurn (actionMelee 0) gs)

        -- Or arrow key movement
        V.EvKey V.KUp       []  -> BMain.continue (fullGameTurn (actionMove ( 0,-1) ) gs)
        V.EvKey V.KDown     []  -> BMain.continue (fullGameTurn (actionMove ( 0, 1) ) gs)
        V.EvKey V.KLeft     []  -> BMain.continue (fullGameTurn (actionMove (-1, 0) ) gs)
        V.EvKey V.KRight    []  -> BMain.continue (fullGameTurn (actionMove ( 1, 0) ) gs)

        -- for a key which does nothing, do nothing (redraw identical)
        -- could optionally print "That key does nothing!" or something?
        _                   -> BMain.continue gs

-- Wasn't even a T.VtyEvent
handleInput gs _ = BMain.continue gs

actionNothing :: Action
actionNothing gs = ActionResult { _costsTurn = False, _gameState = gs }

-- The player has requested to move in some direction.  Return an Action function which attempts the specific move
handleMoveInput :: Char -> Action
handleMoveInput k =
    case k of
        '1' -> actionMove (-1, 1)
        '2' -> actionMove ( 0, 1)
        '3' -> actionMove ( 1, 1)

        '4' -> actionMove (-1, 0)
        '5' -> actionNothing
        '6' -> actionMove ( 1, 0)

        '7' -> actionMove (-1,-1)
        '8' -> actionMove ( 0,-1)
        '9' -> actionMove ( 1,-1)
        _   -> error "non-numeric input to handleMoveInput"


-- run a full game turn, ie pre-turn, player turn, enemy turn, post-turn.
-- Currently, only the player exists.
fullGameTurn :: Action -> GameState -> GameState
fullGameTurn action gs

        -- Player hasn't spent turn - free action or invalid
        | not (result^.costsTurn)   = result^.gameState

        -- Player has spent turn - normal action completed
        -- TODO: this is where to run enemy turns, etc
        | otherwise                 = incrementTurn(result^.gameState)

        where result = playerTurn action gs

incrementTurn :: GameState -> GameState
incrementTurn = over turnNum (+1)

-- The player can do a free action (returns _costsTurn = False) or full action
playerTurn :: Action -> GameState -> ActionResult
playerTurn  action gs =
    -- for the moment, we just do the action.
     let result = action gs in
      --  (result^.gameState)
     result

getPlayer :: GameState -> Creature
getPlayer gs = head $ filter ft (gs^.creatures)
  where
    ft :: Creature -> Bool
    ft c = isJust $ c ^. mPlayerInfo

-- Monsters have no external input, only the game world
monsterTurn :: Creature -> GameState -> GameState
monsterTurn self gs = undefined
  where
    playerPos = (getPlayer gs) ^?! mPlayerInfo

-- Return the Just Creature at the given location, or Nothing if there isn't one
creatureAt :: GameState -> (Int, Int) -> Maybe Creature
creatureAt gs pos =
    find (\m -> (m^.cInfo.position) == pos) (gs^.creatures)

-- Takes current gameState, and direction to move ie (0,1)
-- Maybe we moved, or maybe we print "you can't move there!", etc
-- GameState should be the last argument of every Action, to make it easier to partially apply!
--      Or just use the Action alias
actionMove :: (Int, Int) -> Action
actionMove move gs

    -- Check if that's actually a valid move, ie not into an enemy or wall
    -- If there's a monster there, melee it!
    | isJust $ creatureAt gs attempt  = actionMelee (fromJust $ creatureAt gs attempt) gs
    | targetTile^.walkable  = result_success
    | otherwise             = result_fail

    -- Message string, did it cost the turn, new gamestate
    -- Moving will not have a message later, just for testing.
    --ActionResult  True (addMessage "player moved" resulting_gs)
    -- Return new gameState with message added
    where
        playerPos       = player.cInfo.position      -- A lens, ie gs^.playerPos
        resulting_gs    = over playerPos (addPos move) gs :: GameState
        attempt         = addPos (gs^.player.cInfo.position) move   :: (Int, Int)

        -- TODO: Safer checks than fromJust?
        targetTile      = fromJust $ M.lookup attempt (gs^.gameBoard.tiles)

        -- No message for moving, too spammy
        result_success  = ActionResult True  resulting_gs
        -- Return unchanged gs + message
        result_fail     = ActionResult False (addMessage "That's a wall!" gs)


-- Player (attempts to) attack the specified monster
actionMelee :: Creature -> Action
actionMelee target gs

    | inMeleeRange (gs^.player.cInfo.position) (target^.cInfo.position) = result_success
    | otherwise = result_fail
    where
        -- Note that we add the message first, then damage, so as to come before the death message, etc
        result_success  = ActionResult True  (damageMonster (addMessage "attacked!" gs) target Physical dmg)
        result_fail     = ActionResult False ( addMessage "Out of range!" gs)
        dmg = 1
--        dmg             = fst (rng_result)
--        rng_result      = randomR (1,6) (gs^.rng) :: (Int, StdGen)
--        new_gs          = over (rng) (set (snd rng_result)) gs

-- Rng, number of dice, sides of dice.
-- ie, dieRoll rng 2 6 = 2d6
--dieRoll :: StdGen -> Int -> Int -> (Int, StdGen)
--dieRoll _ 0 _ = 0
-- TODO
-- dieRoll rng n size =




inMeleeRange :: (Int, Int) -> (Int, Int) -> Bool
inMeleeRange one two =
    (abs (fst diff) <= 1) && (abs (snd diff) <= 1)
    where
        diff = one `subtractPos` two
        --prng  = gs^.rng

replaceMonster :: GameState -> Creature -> Creature -> GameState
replaceMonster gs old new = over creatures (map (\i -> if i == old then new else i)) gs

-- gs, MonsterIndex, damage
damageMonster :: GameState -> Creature -> DamageType -> Int -> GameState
damageMonster gs target dmgType dmg

    -- If monster is dead now, delete from list instead of changing it
    | newMonster^.cInfo.health.current <= 0 = over creatures (delete target) (addMessage kill_message  gs)

    -- replace monster with updated monster
    | otherwise = replaceMonster gs target newMonster

    where
        newMonster = over cInfo (damage dmgType dmg) target
        -- Debugging message:
        --message = "Monster was at: " ++ (show $ oldMonster^.mInfo.health.current) ++ "and is now at: " ++ (show $ newMonster^.mInfo.health.current) :: String
        kill_message = "You kill the " ++ fromJust (target^.mMonsterInfo)^.name ++ "!"

-- Universal damage function: will calculate armor, etc
-- Because it operates on CreatureInfo's, it works for both players and monsters.
-- Try to keep the complexity in here
damage  :: DamageType -> Int -> CreatureInfo -> CreatureInfo
damage dmgType dmg oldCreature =
    -- TODO: Armor, etc using dmgType
    newCreature
    where
        newCreature = over (health.current) (subtract dmg) oldCreature

-- Append a message to the gamestate's buffer
-- TODO: Drop old messages once it gets too long
addMessage :: String -> GameState -> GameState
addMessage message gs
    -- TODO: Fix hardcoded buffer length
    | length (gs^.messages) > 12    = over messages (drop 1) added
    | otherwise                     = added
    where
        added   = over messages ( ++ [message]) gs :: GameState
