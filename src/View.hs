module View (drawUI) where

import qualified Brick.Types as T (BrickEvent (..), EventM, Next, Widget)
import Model
import qualified UI.Editor as Editor (render)

drawUI :: State -> [T.Widget Widget]
drawUI s = [Editor.render s]

--   [ hBox
--       [ padLeftRight 1 (Sheet.render s)
--       , padLeftRight 1 (vBox [Stats.render s, Editor.render s])
--       ]
--   ]
