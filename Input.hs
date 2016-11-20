{-# Language TemplateHaskell #-} -- For Lenses

-- export only the top-level, complete result
module Input
( handleInput
)
where


import qualified Graphics.Vty as V
import qualified Brick.Main as BMain
import qualified Brick.Types as T


-- external libraries:
import Control.Lens

-- My own files:
import GameState

--  The structure that all Actions must return
data ActionResult = ActionResult    { _message      :: String -- A one-line string saying what happened

                                    -- Has the player spent his turn? eg an invalid action doesn't count
                                    -- Also enables 'free actions' before your turn, in future
                                    , _costsTurn    :: Bool

                                    -- Resulting gameState - message gets merged into gamestate by 'fullGameTurn'
                                    , _gameState    :: GameState
                                    }

makeLenses '' ActionResult


-- Any action the player can attempt - just an alias
type Action = (GameState -> ActionResult)


handleInput :: GameState -> V.Event -> T.EventM () (T.Next (GameState))
handleInput gs ev =
    case ev of
        -- the empty list [] is the list of mod keys
        V.EvKey V.KEsc []   -> BMain.halt gs    -- press ESC to quit

        -- How to read normal keys:
        -- V.EvKey (V.KChar 'a') [] ->

        -- TODO: Refactor movement into it's own function, matching here on all 4/8 directions?
        -- Can I do a group pattern match like that?
        -- Maybe use numpad and match on number range!
        V.EvKey V.KUp       []  -> BMain.continue (fullGameTurn (playerMove ( 0,-1) ) gs)
        V.EvKey V.KDown     []  -> BMain.continue (fullGameTurn (playerMove ( 0, 1) ) gs)
        V.EvKey V.KLeft     []  -> BMain.continue (fullGameTurn (playerMove (-1, 0) ) gs)
        V.EvKey V.KRight    []  -> BMain.continue (fullGameTurn (playerMove ( 1, 0) ) gs)

        -- for a key which does nothing, do nothing (redraw identical)
        -- could optionally print "That key does nothing!" or something?
        _                   -> BMain.continue gs


-- run a full game turn, ie pre-turn, player turn, enemy turn, post-turn.
-- Currently, only the player exists.
fullGameTurn :: (Action) -> GameState -> GameState
fullGameTurn action gs =
    let result = playerTurn action gs in

        -- Return new gameState
        result^.gameState

        -- TODO: this is where to run enemy turns, etc
        -- TODO: add a message-buffer to GameState, and store the message there
        --      so it can be printed by Draw


-- Arguments:
-- current gameState,
-- an action (function) which a player character attempts to do, ie move, or attack
-- Note that this means the player can only do one thing/keypress per turn, an acceptable limitation
--      Actually, since I have 'costsTurn' now, I have support for 'free actions' before a full action!
playerTurn :: (Action) -> GameState -> ActionResult
playerTurn  action gs =
    -- for the moment, we just do the action.
    action gs

-- Takes current gameState, and direction to move ie (0,1)
-- Maybe we moved, or maybe we print "you can't move there!", etc
-- GameState should be the last argument, to make it easier to partially apply!
playerMove :: (Int, Int) -> GameState -> ActionResult
playerMove move gs  =
    -- TODO: Check if that's actually a valid move, ie not into an enemy or wall
    ActionResult "player moved" True resulting_gs
    where
        resulting_gs = over (player.cInfo.position) (addPos move) gs


