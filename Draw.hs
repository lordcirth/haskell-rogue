module Draw
    ( drawUI
    )
where

-- external libraries:
import Control.Lens
import qualified Data.Map as M
import Data.Maybe   -- fromJust

import Brick.Types
    ( Widget
    , Location(..)
    )

import Brick.Widgets.Core
    (  str     -- :: String -> Widget ()
    , translateBy
    )

-- My own files:
import GameState


-- return the list of UI elements (Widgets)
drawUI :: GameState -> [Widget ()]
drawUI gs =
    [printTurnNumber gs, printMessages gs, drawBoard gs]


drawBoard :: GameState -> Widget ()
drawBoard gs =
    let offset = Location (3,2) in
    translateBy offset (str $ boardAsString gs)


boardAsString :: GameState -> String
boardAsString gs =
    stringGrid (board^.size_x) (board^.size_y) $ charMap gs
    where
        board = gs^.gameBoard -- A lens


charMap :: GameState -> M.Map (Int, Int) Char
charMap gs =
    addPlayer gs (addMonsters gs ground)
    where
        ground = fmap (renderTile) (gs^.gameBoard.tiles)


addPlayer :: GameState -> M.Map (Int, Int) Char -> M.Map (Int, Int) Char
addPlayer gs chars =
    M.insert (playerLocation) (playerChar) chars
    where
        playerChar      = gs^.player.pInfo.cDisplay -- what character to show
        playerLocation  = gs^.player.pInfo.position -- where to render it

addMonsters :: GameState -> M.Map (Int, Int) Char -> M.Map (Int, Int) Char
addMonsters gs chars =
    foldl (addMonster) (chars) (gs^.monsters)
--    M.insert (playerLocation) (playerChar) chars
    where
  --      monsterChar     = monster.mInfo.cDisplay -- what character to show
    --    monsterLocation = monster.mInfo.position -- where to render it

addMonster ::  M.Map (Int, Int) Char -> Monster -> M.Map (Int, Int) Char
addMonster chars monster    =
    M.insert (monster^.mInfo.position) (monster^.mInfo.cDisplay) chars


-- This is probably bad code, but it does work...
-- The order we print it in here basically determines what the coord system is
-- (1,1) is the top-left corner
stringGrid :: Int -> Int -> M.Map (Int, Int) Char -> String
stringGrid sizeX sizeY mapGrid =
    unlines $ chop sizeX string
    where
        -- generate all (x,y)'s, look them up, and put them in a string
        -- put Y first so that X iterates first, making X horizontal and Y vertical
        string = [ fromJust $ M.lookup (x,y) mapGrid | y <- [1..sizeY], x <- [1..sizeX] ] :: String

-- used in stringGrid
chop :: Int -> String -> [String]
chop _      []       = []
chop sizeX  string   =
    (fst sParts):(chop sizeX $ snd sParts)
    where
        sParts = splitAt sizeX string


-- Later we'll need to overlay multiple terrain effects, maybe?
-- creatures, etc are rendered elsewhere, ie addPlayer
renderTile :: Tile -> Char
renderTile tile = tile^.tDisplay


-- print the message buffer
printMessages :: GameState -> Widget ()
printMessages gs =
    let offset = Location (0,gs^.gameBoard.size_x + 4) in
    translateBy offset (str $ unlines (gs^.messages))

printTurnNumber :: GameState -> Widget ()
printTurnNumber gs =
    let offset = Location (gs^.gameBoard.size_y + 4, 0)
        turnString = "Turn: " ++ (show $ gs^.turnNum)
    in

    translateBy offset (str $ turnString)
