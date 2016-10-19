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
    [drawBoard $ GS.gameBoard gs]


drawBoard :: GS.Board -> Widget ()
drawBoard board =
    str $ boardAsString board

boardAsString :: GS.Board -> String
boardAsString board =
    stringGrid (GS.x board) (GS.y board) charMap
    where
        charMap = fmap (renderTile) $ GS.tiles board

-- This is probably bad
stringGrid :: Int -> Int -> M.Map (Int, Int) Char -> String
stringGrid sizeX sizeY mapGrid =
    unlines $ chop sizeX string
    where
       string = [ fromJust $ M.lookup (x,y) mapGrid | x <- [1..sizeX], y <- [1..sizeY] ] :: String
-- TODO: Split, then unlines

chop :: Int -> String -> [String]
chop sizeX []       = []
chop sizeX string   =
    (fst parts):(chop sizeX $ snd parts)
    where
        parts = splitAt sizeX string


-- Later we'll need to overlay creatures, etc
renderTile :: GS.Tile -> Char
renderTile tile =
    GS.display tile

-- D.Dialog Class = gameState passed by Brick!
-- ui_chooseClass :: D.Dialog Class -> Widget ()
-- ui_chooseClass classChoice =
--    D.renderDialog classChoice $ str "Choose your class"

