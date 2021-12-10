module UI.Clock
  ( render,
  )
where

import Brick (strWrap)
import Brick.AttrMap (AttrName, attrName)
import qualified Brick.AttrMap as A (applyAttrMappings)
import qualified Brick.Types as T (Padding (..), Widget)
import Brick.Util (on)
import qualified Brick.Widgets.Border as B (border, borderAttr, hBorder)
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
import Data.Bool (bool)
import Data.Maybe
import Data.Time.Calendar (Day)
import qualified Data.Time.Format as F (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (ZonedTime (..), getZonedTime)
import qualified Graphics.Vty as V (black, defAttr, magenta)
import Lib.Digit (secondsToDigitLines)
import Model (State (..), Status (..), Task (..), Widget (..))
import UI.Style (bold)

render :: State -> T.Widget Widget
render s =
  (drawSessionTitle s)
    <=> padBottom T.Max (C.hCenter $ C.center (drawClock s))
    <=> drawPause s

renderNumbers :: [String] -> String
renderNumbers ss = replaceDot (replaceHash (unlines ss))
  where
    replaceHash = map (\c -> if c == '#' then '█' else c)
    replaceDot = map (\c -> if c == '.' then ' ' else c)

drawSessionTitle :: State -> T.Widget Widget
drawSessionTitle s =
  withBorderStyle unicodeRounded . B.border . C.hCenter . padTopBottom 1 $
    vBox
      [ C.hCenter . str $
          ( case status s of
              Running -> "Hamodoro Focus Session"
              Paused -> "Session Paused"
              _ -> "ERROR STATE"
          ),
        withAttr bold (C.hCenter . str $ title (fromJust $ task s))
      ]

drawClock :: State -> T.Widget Widget
drawClock s =
  withBorderStyle unicodeRounded . B.border . C.hCenter . padTopBottom 1 $
    hBox [str $ renderNumbers (secondsToDigitLines $ countdown s)]

drawPause :: State -> T.Widget Widget
drawPause s =
  withBorderStyle unicodeRounded . B.border . C.hCenter $
    ( str "[p]: Pause / Resume timer"
        <=> str "[Esc]: quit"
    )
