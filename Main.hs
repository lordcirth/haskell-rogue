module Main where
-- Init UI, run mainloop

-- Allows string literals to be converted to other types like Text
{-# LANGUAGE OverloadedStrings #-}

-- import other files from this project:
-- Define GameState structure & initialState
import qualified GameState as GS
import qualified Draw as UI


--Brick imports
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.Main as BMain

import Brick.Types
  ( Widget
  )

import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T

--Theming boilerplate
theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    ]


-- Define how Brick should act
theApp :: BMain.App GameState V.Event ()
theApp =
    BMain.App {
        -- UI.drawUI :: gameState -> [Widget]
        BMain.appDraw = UI.drawUI
      , BMain.appChooseCursor = BMain.showFirstCursor

        -- function which takes gameState & an Event and returns mutated gameState
      , BMain.appHandleEvent = handleInput
      , BMain.appStartEvent = return
      , BMain.appAttrMap = const theMap
      , BMain.appLiftVtyEvent = id
      }

handleInput :: D.Dialog Class -> V.Event -> T.EventM () (T.Next (D.Dialog Class))
handleInput d ev =
    case ev of
        V.EvKey V.KEsc [] -> BMain.halt d
        V.EvKey V.KEnter [] -> BMain.halt d
        _ -> BMain.continue =<< D.handleDialogEvent ev d


main :: IO ()
main = do
    dialog <- BMain.defaultMain theApp GS.initialState
    putStrLn $ "You chose: " ++ show (D.dialogSelection dialog)
