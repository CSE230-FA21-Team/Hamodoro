-- View: UI, widgets, etc.

module View (drawUI) where

import qualified Brick.Types as T (BrickEvent (..), EventM, Next, Widget)
import Model
import qualified UI.Editor as Editor (render)
import qualified UI.Schedule as Schedule (render)

import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , padBottom
  , padTopBottom
  , padLeftRight
  , padLeft
  , str
  , updateAttrMap
  , vBox
  , hBox
  , withAttr
  , withBorderStyle
  )

drawUI :: State -> [T.Widget Widget]
--drawUI s = [Editor.render s]
drawUI s  = [hBox
                [ padLeftRight 1 (Editor.render s)
                , padLeftRight 1 (Schedule.render s)
                ] 
            ]


--   [ hBox
--       [ padLeftRight 1 (Sheet.render s)
--       , padLeftRight 1 (vBox [Stats.render s, Editor.render s])
--       ]
--   ]
