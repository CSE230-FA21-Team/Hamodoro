module UI.Ending
  ( render,
  )
where

import qualified Brick.Types as T (Widget)
import qualified Brick.Widgets.Border as B (border)
import Brick.Widgets.Border.Style (unicodeRounded)
import qualified Brick.Widgets.Center as C (center, hCenter)
import Brick.Widgets.Core
  ( hBox,
    padBottom,
    padLeft,
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

render :: State -> T.Widget Widget
render s =
  (drawFinish s)
    <=> (drawBreak s)
    <=> (drawNew s)

drawFinish :: State -> T.Widget Widget
drawFinish s =
  withBorderStyle unicodeRounded . B.border . C.hCenter . padTopBottom 1 $
    hBox [str $ "Session Finished"]

drawBreak :: State -> T.Widget Widget
drawBreak s =
  withBorderStyle unicodeRounded . B.border . C.hCenter . padTopBottom 5 $
    hBox [str $ "Take a break ∠( ᐛ 」∠)＿"]

drawNew :: State -> T.Widget Widget
drawNew s =
  withBorderStyle unicodeRounded . B.border . C.hCenter $
    hBox [str $ "New Session (N)"]
