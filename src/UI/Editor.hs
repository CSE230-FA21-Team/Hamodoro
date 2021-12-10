module UI.Editor
  ( render,
  )
where

import qualified Brick.Focus as F
import qualified Brick.Types as T (Widget)
import qualified Brick.Widgets.Border as B (border)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Core
  ( hLimit,
    str,
    vLimit,
    withBorderStyle,
    (<+>),
    (<=>),
    hBox,
    vBox
  )
import qualified Brick.Widgets.Center as C (center, hCenter)
import Brick.Widgets.Edit (renderEditor)
import Model

render :: State -> T.Widget Widget
render s =
  (drawTitle s)
  <=>
  (drawNotes s)
  <=>
  (drawDuration s)
  <=>
  (drawNotification s)

drawTitle :: State -> T.Widget Widget
drawTitle s =
  withBorderStyle unicodeRounded . B.border $
    hBox [str "Task Title: " <+> vLimit 2 e1]
    where
    e1 = F.withFocusRing (_focusRing s) (renderEditor (str . unlines)) (_editor1 s)

drawNotes :: State -> T.Widget Widget
drawNotes s =
  withBorderStyle unicodeRounded . B.border $
    vBox [str "Notes: " <=> e2]
    where
    e2 = F.withFocusRing (_focusRing s) (renderEditor (str . unlines)) (_editor2 s)

drawDuration :: State -> T.Widget Widget
drawDuration s =
  withBorderStyle unicodeRounded . B.border $
    hBox [str "Duration: " <+> vLimit 1 e3
    <=> str (notification s)]
    where
    e3 = F.withFocusRing (_focusRing s) (renderEditor (str . unlines)) (_editor3 s)

drawNotification :: State -> T.Widget Widget
drawNotification s =
  withBorderStyle unicodeRounded . B.border . C.hCenter $
    (str "[Up/Down]: switch between editors"
    <=> str "[Tab]: finish editing, then [Enter] to start session"
    <=> str "[Esc]: quit")
