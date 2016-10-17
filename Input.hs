module Input
( handleInput
)
where

import qualified GameState as GS

--Brick imports
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Dialog as D
-- import qualified Brick.Widgets.Center as C
import qualified Brick.Main as BMain
import qualified Brick.Types as T

handleInput :: GS.GameState -> V.Event -> T.EventM () (T.Next (GS.GameState))
handleInput gs ev =
    case ev of
        V.EvKey V.KEsc [] -> BMain.halt gs
        V.EvKey V.KEnter [] -> BMain.halt gs

        --else do nothing
        _ -> BMain.continue gs
