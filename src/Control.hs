-- Control: operations and actions

module Control where

import Brick hiding (Result)
-- ( Tick,
--   Panel(..),
--   State(..),
--   editor,
--   Widget(..),
--   )

import Brick.BChan (BChan, writeBChan)
import qualified Brick.Focus as F
import qualified Brick.Main as M
  ( App (..),
    appAttrMap,
    appChooseCursor,
    appDraw,
    appHandleEvent,
    appStartEvent,
    continue,
    customMain,
    halt,
    showFirstCursor,
  )
import qualified Brick.Types as T
--import qualified Brick.Types as T (BrickEvent (..), EventM, Next, handleEventLensed)
import qualified Brick.Widgets.Edit as E
  ( applyEdit,
    editor,
    getEditContents,
    handleEditorEvent,
  )
import Config (Config)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (find, intercalate)
import Data.Time.Clock (DiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds, utctDay)
import Data.Time.LocalTime (ZonedTime (..), getCurrentTimeZone, getZonedTime, utcToLocalTime, utcToZonedTime, zonedTimeToUTC)
import qualified Graphics.Vty as V
import Lens.Micro
import Lib
import Model
import Text.Read

-- import Model.Player

-------------------------------------------------------------------------------

control :: State -> T.BrickEvent Model.Widget Tick -> EventM Model.Widget (T.Next State)
control s@State {_panel = p} (T.VtyEvent ev) =
  case (p, ev) of
    --   AppEvent Tick -> nextS s =<< liftIO (play O s)
    -- AppEvent Tick -> M.continue =<< liftIO (autoRefresh s)
    -- AppEvent s (T.AppEvent Tick) = M.continue =<< liftIO (autoRefresh s)
    (Editor, V.EvKey V.KEsc _) -> M.halt s
    --(Editor, V.EvKey (V.KChar '\t') _) -> M.continue (s {_focusRing = F.focusNext (_focusRing s)})
    (Editor, V.EvKey (V.KChar '\t') _) -> M.continue (s {_panel = Schedule})
    (Editor, V.EvKey V.KDown _) -> M.continue (s {_focusRing = F.focusNext (_focusRing s)})
    (Editor, V.EvKey V.KUp _) -> M.continue (s {_focusRing = F.focusPrev (_focusRing s)})
    (Editor, V.EvKey V.KEnter _) -> M.continue =<< liftIO (save s)
    (Editor, _) ->
      M.continue =<< case F.focusGetCurrent (_focusRing s) of
        Just Edit1 -> T.handleEventLensed s editor1 E.handleEditorEvent ev
        Just Edit2 -> T.handleEventLensed s editor2 E.handleEditorEvent ev
        Just Edit3 -> T.handleEventLensed s editor3 E.handleEditorEvent ev
        Nothing -> return s
    (Schedule, V.EvKey (V.KChar 'C') _) -> M.continue =<< liftIO (clear s)
    (Schedule, V.EvKey (V.KChar 'D') _) -> M.continue =<< liftIO (deleteOne s)
    (Schedule, V.EvKey V.KEsc _) -> M.halt s
    (Schedule, V.EvKey (V.KChar '\t') _) -> M.continue (s {_panel = Editor})
    (Schedule, _) -> M.continue s
    (Clock, V.EvKey V.KEsc _) -> M.halt s
    (Clock, _) -> M.continue s
control s (T.AppEvent Tick) = M.continue =<< liftIO (autoRefresh s)
control s _ = M.continue s -- Brick.halt s

clear :: State -> IO State
clear s = do
  pure $
    s
      { tasks = []
      }

deleteOne :: State -> IO State
deleteOne s@State {tasks = ts} = do
  pure $
    s
      { tasks = if (length ts) >= 1 then (tail ts) else []
      }

save :: State -> IO State
save s@State {tasks = ts, _editor1 = ed1, _editor2 = ed2, _editor3 = ed3} = do
  tz <- getCurrentTimeZone
  utcTime <- getCurrentTime

  let title1 = intercalate "; " . filter (/= "") $ E.getEditContents ed1
      notes1 = intercalate "; " . filter (/= "") $ E.getEditContents ed2
      zTime = utcToZonedTime tz utcTime
      duration1 = (E.getEditContents ed3) !! 0
  --case readMaybe duration1 of
  let duration_int = parseIntOrDefault duration1 (-1)
  let endTime = utcToZonedTime tz (addUTCTime (fromIntegral duration_int * 60) utcTime)

  if duration_int >= 100 || duration_int <= 0
    then pure $ s {notification = "Please enter a number between 1-99!"}
    else
      pure $
        s
          { status = Running,
            notification = " ",
            _panel = Clock,
            tasks =
              ts
                ++ [ Task
                       { title = title1,
                         notes = notes1,
                         duration = read duration1,
                         startTime = zTime,
                         endTime = endTime
                       }
                   ]
          }

--where title1 = intercalate "; " . filter (/= "") $ E.getEditContents ed1
--notes1 = intercalate "; " . filter (/= "") $ E.getEditContents ed2
--duration1 = (E.getEditContents ed3) !! 0

--startTime = zoneT,
--endTime = zoneT}

--editToSchedule :: State -> State
--editToSchedule s = s & panel .~ Schedule

autoRefresh :: State -> IO State
autoRefresh s = do
  d <- getCurrentTime
  if status s /= Running
    then pure s
    else do
      latestTask <- getLatestTask s
      let latestEndTime = zonedTimeToUTC (endTime latestTask)
          diff = nominalDiffTimeToSeconds (diffUTCTime latestEndTime d)
      if diff > 0
        then
          pure $
            s
              { now = d,
                countdown = floor $ toRational $ diff
              }
        else pure $ s {now = d}

getLatestTask :: State -> IO Task
getLatestTask s = do
  let ts = tasks s
  let t = last ts
  pure t

syncFetch :: Config -> IO (BChan Tick -> State)
syncFetch c = do
  d <- getCurrentTime
  z <- getZonedTime
  pure $ \q ->
    State
      { config = c,
        _panel = Editor,
        status = Ready,
        _editor1 = E.editor Edit1 Nothing "",
        _editor2 = E.editor Edit2 Nothing "",
        _editor3 = E.editor Edit3 Nothing "0",
        _focusRing = F.focusRing [Edit1, Edit2, Edit3],
        notification = " ",
        now = d,
        day = (utctDay d),
        countdown = 0,
        -- TODO: change tasks back to []
        tasks =
          [ Task
              { title = "test task 1",
                notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                duration = 20,
                startTime = z,
                endTime = z
              },
            Task
              { title = "test task 1",
                notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                duration = 20,
                startTime = z,
                endTime = z
              },
            Task
              { title = "test task 1",
                notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                duration = 20,
                startTime = z,
                endTime = z
              },
            Task
              { title = "test task 1",
                notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                duration = 20,
                startTime = z,
                endTime = z
              },
            Task
              { title = "test task 1",
                notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                duration = 20,
                startTime = z,
                endTime = z
              },
            Task
              { title = "test task 1",
                notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                duration = 20,
                startTime = z,
                endTime = z
              },
            Task
              { title = "test task 1",
                notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                duration = 20,
                startTime = z,
                endTime = z
              },
            Task
              { title = "test task 1",
                notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                duration = 20,
                startTime = z,
                endTime = z
              },
            Task
              { title = "test task 1",
                notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                duration = 20,
                startTime = z,
                endTime = z
              }
          ]
      }

renderNotes :: String -> String
renderNotes = unlines . filter (/= "") . map trimLeft . splitOn ';'

appCursor :: State -> [T.CursorLocation Model.Widget] -> Maybe (T.CursorLocation Model.Widget)
appCursor = F.focusRingCursor (_focusRing)
