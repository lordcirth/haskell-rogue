module Draw
    ( drawUI
    )
where

-- external libraries:
import Control.Lens
import qualified Data.Map as M
import Data.Maybe

import Brick.Types
  ( Widget
  )

import Brick.Widgets.Core
  ( padAll
  , str     -- :: String -> Widget ()

  )

-- My own files:
import GameState


-- return the list of UI elements (Widgets)
drawUI :: GameState -> [Widget ()]
drawUI gs =
    [drawBoard gs]


drawBoard :: GameState -> Widget ()
drawBoard gs =
    str $ boardAsString gs


boardAsString :: GameState -> String
boardAsString gs =
    stringGrid (board^.x) (board^.y) $ charMap gs
    where
        board = gs^.gameBoard -- A lens


--TODO: Start using Lenses!
charMap :: GameState -> M.Map (Int, Int) Char
charMap gs =
    addPlayer gs ground
    where
        ground = fmap (renderTile) (gs^.gameBoard.tiles)


addPlayer :: GameState -> M.Map (Int, Int) Char -> M.Map (Int, Int) Char
addPlayer gs chars =
    M.insert (playerLocation) (playerChar) chars
    where
        playerChar      = gs^.player.cInfo.cDisplay -- what character to show
        playerLocation  = gs^.player.cInfo.position -- where to render it


-- This is probably bad code, but it does work...
stringGrid :: Int -> Int -> M.Map (Int, Int) Char -> String
stringGrid sizeX sizeY mapGrid =
    unlines $ chop sizeX string
    where
       string = [ fromJust $ M.lookup (x,y) mapGrid | x <- [1..sizeX], y <- [1..sizeY] ] :: String

-- used in stringGrid
chop :: Int -> String -> [String]
chop sizeX []       = []
chop sizeX string   =
    (fst parts):(chop sizeX $ snd parts)
    where
        parts = splitAt sizeX string


-- Later we'll need to overlay multiple terrain effects, maybe?
-- creatures, etc are rendered elsewhere, ie addPlayer
renderTile :: Tile -> Char
renderTile tile = tile^.tDisplay
