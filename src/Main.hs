module Main where
-- Init UI, run mainloop

-- Allows string literals to be converted to other types like Text
{-# LANGUAGE OverloadedStrings #-}

-- import other files from this project:
-- Define GameState structure & initialState
import qualified GameState as GS
import qualified Draw as UI
import qualified Input as IN


--Brick imports
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Main as BMain
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)


--RNG
import System.Random

--Theming boilerplate
theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    ]


-- Define how Brick should act
-- data App s e n
-- s is state type; GameState
-- e is event type; unused e
-- n is resource name type; ()
theApp :: BMain.App GS.GameState e ()
theApp =
    BMain.App {
        -- UI.drawUI :: gameState -> [Widget]
        BMain.appDraw = UI.drawUI
      , BMain.appChooseCursor = BMain.showFirstCursor

        -- function which takes gameState & an Event and returns mutated gameState
      , BMain.appHandleEvent = IN.handleInput
      , BMain.appStartEvent = return
      , BMain.appAttrMap = const theMap
      }

main :: IO ()
main = do
    rng <- getStdGen
    dialog <- BMain.defaultMain theApp (GS.initialState rng)
    putStrLn $ "End"
