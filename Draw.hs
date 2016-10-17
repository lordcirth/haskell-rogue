import qualified GameState as GS

-- return the list of UI elements (Widgets)
drawUI :: GS.GameState -> [Widget ()]
drawUI gs = 
    [drawBoard $ gameBoard gs]


drawBoard :: Board -> Widget ()
drawBoard =



-- D.Dialog Class = gameState passed by Brick!
-- ui_chooseClass :: D.Dialog Class -> Widget ()
-- ui_chooseClass classChoice = 
--    D.renderDialog classChoice $ str "Choose your class"

