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
  )
import Brick.Widgets.Edit (renderEditor)
import Model

render :: State -> T.Widget Widget
render s =
  withBorderStyle unicodeRounded . B.border $
    (str "Task Title: " <+> (hLimit 20 $ vLimit 5 e1))
      <=> str " "
      <=> (str "Notes: " <+> (hLimit 30 e2))
      <=> str " "
      <=> (str "Duration: " <+> (hLimit 4 e3))
      <=> str " "
      <=> str (notification s)
      <=> str "Press Tab to switch between editors."
      <=> str "Press Esc to quit."
  where
    e1 = F.withFocusRing (_focusRing s) (renderEditor (str . unlines)) (_editor1 s)
    e2 = F.withFocusRing (_focusRing s) (renderEditor (str . unlines)) (_editor2 s)
    e3 = F.withFocusRing (_focusRing s) (renderEditor (str . unlines)) (_editor3 s)

--renderEditor (str . unlines) (True) (_editor s)
-- <=>
--withBorderStyle unicodeRounded . B.border $
--  renderEditor (str . unlines) (True) (_editorN s)
-- <=>
--withBorderStyle unicodeRounded . B.border $
--  renderEditor (str . unlines) (True) (_editorD s)
