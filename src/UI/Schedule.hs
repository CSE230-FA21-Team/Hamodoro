module UI.Schedule
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
import Data.Time.LocalTime (ZonedTime (..), getZonedTime, utcToZonedTime)
import qualified Graphics.Vty as V (black, defAttr, magenta)
import Graphics.Vty.Attributes
import Model
  ( State (State, day, now, tasks),
    Task (duration, endTime, notes, startTime, title),
    Widget,
  )

render :: State -> T.Widget Widget
render s =
  (drawDate s)
    <=> padBottom
      T.Max
      ( C.hCenter $
          vBox (drawTasks s `orEmpty` [C.center $ str "No Task Done Yet"])
      )
    <=> (drawClear s)

drawDate :: State -> T.Widget Widget
drawDate s =
  withBorderStyle unicodeRounded . B.border . C.hCenter . padTopBottom 1 $
    hBox [str (formatDate $ day s)]

drawDelete :: State -> T.Widget Widget
drawDelete s =
  withBorderStyle unicodeRounded . B.border . C.hCenter $
    hBox [str $ "Delete the earliest task (d)"]

drawClear :: State -> T.Widget Widget
drawClear s =
  withBorderStyle unicodeRounded . B.border . C.hCenter $
    hBox [str $ "Clear all tasks (c)"]

drawTasks :: State -> [T.Widget Widget]
drawTasks s@State {tasks = ts} = map drawTask (reverse ts)

drawTask :: Task -> T.Widget Widget
drawTask t =
  taskStyle True . withBorderStyle unicodeRounded . B.border $
    (sessionTitle <+> dur) <=> B.hBorder <=> note <=> time
  where
    sessionTitle = (withAttr (attrName "bold") . str . title) t
    dur = padLeft T.Max . str $ show (duration t) ++ " min"
    note = padBottom (T.Pad 1) (strWrap $ (notes t))
    time = padLeft T.Max . str $ (formatTime $ startTime t) ++ " to " ++ (formatTime $ endTime t)

orEmpty :: (Foldable f) => f a -> f a -> f a
orEmpty a b = bool a b (null a)

formatTime :: ZonedTime -> String
formatTime = F.formatTime F.defaultTimeLocale "%m/%d %R"

formatDate :: Day -> String
formatDate = F.formatTime F.defaultTimeLocale "%F"

taskStyle :: Bool -> T.Widget Widget -> T.Widget Widget
taskStyle True =
  updateAttrMap (A.applyAttrMappings [(B.borderAttr, (V.defAttr `withForeColor` V.magenta))])
taskStyle False = id
