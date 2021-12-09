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
import Data.Time.Calendar (Day)
import qualified Data.Time.Format as F (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (ZonedTime (..), getZonedTime)
import qualified Graphics.Vty as V (black, defAttr, magenta)
import Graphics.Vty.Attributes
import Lib.Digit
import Model (State (..), Task (..), Widget (..))

render :: State -> T.Widget Widget
render s = C.hCenter $ C.center (drawClock s)

renderNumbers :: [String] -> String
renderNumbers ss = replaceDot (replaceHash (unlines ss))
  where
    replaceHash = map (\c -> if c == '#' then '█' else c)
    replaceDot = map (\c -> if c == '.' then ' ' else c)

drawClock :: State -> T.Widget Widget
drawClock s =
  withBorderStyle unicodeRounded . B.border . C.hCenter . padTopBottom 1 $
    hBox [str $ renderNumbers (timeToDigitalLines [1, 2, 3, 4])]
