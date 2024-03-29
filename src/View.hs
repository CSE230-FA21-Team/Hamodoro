-- View: UI, widgets, etc.

module View (drawUI) where

import qualified Brick.Types as T (BrickEvent (..), EventM, Next, Widget)
import Brick.Widgets.Core
  ( hBox,
    padBottom,
    padLeft,
    padLeftRight,
    padTopBottom,
    str,
    updateAttrMap,
    vBox,
    withAttr,
    withBorderStyle,
    (<+>),
    (<=>),
  )
import Model
import qualified UI.Clock as Clock (render)
import qualified UI.Editor as Editor (render)
import qualified UI.Ending as Ending (render)
import qualified UI.Schedule as Schedule (render)

drawUI :: State -> [T.Widget Widget]
--drawUI s = [Editor.render s]
drawUI s =
  case status s of
    Ready ->
      [ hBox
          [ padLeftRight 1 (Editor.render s),
            padLeftRight 1 (Schedule.render s)
          ]
      ]
    Paused ->
      [ hBox
          [ padLeftRight 1 (Clock.render s),
            padLeftRight 1 (Schedule.render s)
          ]
      ]
    Running ->
      [ hBox
          [ padLeftRight 1 (Clock.render s),
            padLeftRight 1 (Schedule.render s)
          ]
      ]
    Finished ->
      [ hBox
          [ padLeftRight 1 (Ending.render s),
            padLeftRight 1 (Schedule.render s)
          ]
      ]
