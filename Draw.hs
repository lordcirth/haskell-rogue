module Draw
    ( drawUI
    )
where

import qualified Data.Map as M
import qualified GameState as GS
import Data.Maybe
-- import qualified Graphics.Vty as V
-- import qualified Brick.Widgets.Dialog as D
-- import qualified Brick.Widgets.Center as C
-- import qualified Brick.Main as BMain

import Brick.Types
  ( Widget
  )

import Brick.Widgets.Core
  ( padAll
  , str     -- :: String -> Widget ()

  )

-- return the list of UI elements (Widgets)
drawUI :: GS.GameState -> [Widget ()]
drawUI gs =
    [drawBoard gs]


drawBoard :: GS.GameState -> Widget ()
drawBoard gs =
    str $ boardAsString gs


boardAsString :: GS.GameState -> String
boardAsString gs =
    stringGrid (x) (y) $ charMap gs
    where
       -- charMap = fmap (renderTile) $ GS.tiles board
        x       = GS.x $ GS.gameBoard gs
        y       = GS.y $ GS.gameBoard gs

--TODO: Start using Lenses!
charMap :: GS.GameState -> M.Map (Int, Int) Char
charMap gs =
    addPlayer gs ground
    where
        ground = fmap (renderTile) $ GS.tiles $ GS.gameBoard gs

addPlayer :: GS.GameState -> M.Map (Int, Int) Char -> M.Map (Int, Int) Char
addPlayer gs chars =
    -- TODO: no-op so far
   -- GS.tiles $ GS.gameBoard
    M.insert (playerLocation) (playerChar) chars
    where

        playerChar = GS.cDisplay $ GS.cInfo $ GS.player $ gs
        playerLocation = GS.position $ GS.cInfo $ GS.player $ gs

-- This is probably bad
stringGrid :: Int -> Int -> M.Map (Int, Int) Char -> String
stringGrid sizeX sizeY mapGrid =
    unlines $ chop sizeX string
    where
       string = [ fromJust $ M.lookup (x,y) mapGrid | x <- [1..sizeX], y <- [1..sizeY] ] :: String


chop :: Int -> String -> [String]
chop sizeX []       = []
chop sizeX string   =
    (fst parts):(chop sizeX $ snd parts)
    where
        parts = splitAt sizeX string


-- Later we'll need to overlay creatures, etc
renderTile :: GS.Tile -> Char
renderTile tile =
    GS.tDisplay tile
