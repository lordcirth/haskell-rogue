module Main where
-- Init UI, run mainloop

-- Allows string literals to be converted to other types like Text
{-# LANGUAGE OverloadedStrings #-}

-- import other files from this project:
-- Define GameState structure & initialState
import qualified GameState as GS


--Brick imports
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.Main as M

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
theApp :: M.App GameState V.Event ()
theApp =
    M.App { 
        -- drawUI :: gameState -> [Widget]
        M.appDraw = drawUI 
      , M.appChooseCursor = M.showFirstCursor

        -- function which takes gameState & an Event and returns mutated gameState
      , M.appHandleEvent = handleInput 
      , M.appStartEvent = return             
      , M.appAttrMap = const theMap          
      , M.appLiftVtyEvent = id               
      }                                      

handleInput :: D.Dialog Class -> V.Event -> T.EventM () (T.Next (D.Dialog Class))
handleInput d ev =
    case ev of
        V.EvKey V.KEsc [] -> M.halt d
        V.EvKey V.KEnter [] -> M.halt d
        _ -> M.continue =<< D.handleDialogEvent ev d


main :: IO ()
main = do
    dialog <- M.defaultMain theApp GS.initialState
    putStrLn $ "You chose: " ++ show (D.dialogSelection dialog)
