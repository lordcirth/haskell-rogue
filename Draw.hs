import qualified GameState as GS
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
    str "hello world"



-- D.Dialog Class = gameState passed by Brick!
-- ui_chooseClass :: D.Dialog Class -> Widget ()
-- ui_chooseClass classChoice =
--    D.renderDialog classChoice $ str "Choose your class"

