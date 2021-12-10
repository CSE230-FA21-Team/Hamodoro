-- Control: operations and actions

module Control where

import Brick hiding (Result)
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
import qualified Brick.Widgets.Edit as E
  ( applyEdit,
    editor,
    getEditContents,
    handleEditorEvent,
  )
import Config (Config)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (find, intercalate)
import Data.Maybe
import Data.Time.Clock (DiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds, utctDay)
import Data.Time.LocalTime (ZonedTime (..), getCurrentTimeZone, getZonedTime, utcToLocalTime, utcToZonedTime, zonedTimeToUTC)
import qualified Graphics.Vty as V
import Lens.Micro
import Lib
import Model
import System.Info (os)
import System.Process
import Text.Read

-------------------------------------------------------------------------------

control :: State -> T.BrickEvent Model.Widget Tick -> EventM Model.Widget (T.Next State)
control s@State {_panel = p} (T.VtyEvent ev) =
  case (p, ev) of
    (Editor, V.EvKey V.KEsc _) -> M.halt s
    (Editor, V.EvKey (V.KChar '\t') _) -> M.continue (s {_panel = Schedule})
    (Editor, V.EvKey V.KDown _) -> M.continue (s {_focusRing = F.focusNext (_focusRing s)})
    (Editor, V.EvKey V.KUp _) -> M.continue (s {_focusRing = F.focusPrev (_focusRing s)})
    (Editor, _) ->
      M.continue =<< case F.focusGetCurrent (_focusRing s) of
        Just Edit1 -> T.handleEventLensed s editor1 E.handleEditorEvent ev
        Just Edit2 -> T.handleEventLensed s editor2 E.handleEditorEvent ev
        Just Edit3 -> T.handleEventLensed s editor3 E.handleEditorEvent ev
        Nothing -> return s
    (Schedule, V.EvKey (V.KChar 'c') _) -> M.continue =<< liftIO (onClear s)
    (Schedule, V.EvKey V.KEnter _) -> M.continue =<< liftIO (onStart s)
    (Schedule, V.EvKey V.KEsc _) -> M.halt s
    (Schedule, V.EvKey (V.KChar '\t') _) -> M.continue (s {_panel = Editor})
    (Schedule, _) -> M.continue s
    (Clock, V.EvKey V.KEsc _) -> M.halt s
    (Clock, V.EvKey (V.KChar 'p') _) -> M.continue =<< liftIO (onPause s)
    (Clock, _) -> M.continue s
    (Ending, V.EvKey V.KEsc _) -> M.halt s
    (Ending, V.EvKey (V.KChar 'n') _) -> M.continue =<< liftIO (onRestart s)
    (Ending, _) -> M.continue s
control s (T.AppEvent Tick) = M.continue =<< liftIO (onTick s)
control s _ = M.continue s

onClear :: State -> IO State
onClear s =
  pure $
    s
      { tasks = []
      }

onRestart :: State -> IO State
onRestart s =
  pure $
    s
      { status = Ready,
        _panel = Editor
      }

onStart :: State -> IO State
onStart s@State {tasks = ts, _editor1 = ed1, _editor2 = ed2, _editor3 = ed3} = do
  tz <- getCurrentTimeZone
  utcTime <- getCurrentTime

  let title1 = unlines (E.getEditContents ed1)
      notes1 = unlines (E.getEditContents ed2)
      zTime = utcToZonedTime tz utcTime
      duration1 = head (E.getEditContents ed3)
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
            task =
              Just
                Task
                  { title = title1,
                    notes = notes1,
                    duration = read duration1,
                    startTime = zTime,
                    endTime = endTime
                  }
          }

onTick :: State -> IO State
onTick s = do
  d <- getCurrentTime
  if status s /= Running
    then pure s
    else do
      let latestTask = fromJust $ task s
          latestEndTime = zonedTimeToUTC (endTime latestTask)
          diff = nominalDiffTimeToSeconds (diffUTCTime latestEndTime d)
      if diff > 0
        then
          pure $
            s
              { now = d,
                countdown = floor $ toRational $ diff
              }
        else onComplete s

onPause :: State -> IO State
onPause s =
  if status s == Running
    then pure $ s {status = Paused}
    else onResume s

onResume :: State -> IO State
onResume s@State {task = t} = do
  utcTime <- getCurrentTime
  tz <- getCurrentTimeZone
  let endTime = utcToZonedTime tz (addUTCTime (fromIntegral (countdown s)) utcTime)
  -- putStrLn $ "Resuming task at endtime " ++ (show endTime)
  if status s == Paused
    then pure $ s {status = Running, task = Just $ (fromJust t) {endTime = endTime}}
    else pure s

onComplete :: State -> IO State
onComplete s = do
  d <- getCurrentTime
  let t = fromJust $ task s
  if os == "darwin"
    then runCommand "osascript -e 'display notification \"You have finished a focus session! Now take a break.\" with title \"Hamodoro\" subtitle \"Focus Session Complete!\"'"
    else runCommand "notify-send \"You have finished a focus session! Now take a break.\""
  pure $ s {now = d, status = Finished, _panel = Ending, tasks = (tasks s ++ [t])}

initState :: Config -> IO (BChan Tick -> State)
initState c = do
  d <- getCurrentTime
  z <- getZonedTime
  pure $
    const
      State
        { config = c,
          _panel = Editor,
          status = Ready,
          _editor1 = E.editor Edit1 (Just 1) "",
          _editor2 = E.editor Edit2 Nothing "",
          _editor3 = E.editor Edit3 (Just 1) "0",
          _focusRing = F.focusRing [Edit1, Edit2, Edit3],
          notification = " ",
          now = d,
          day = utctDay d,
          countdown = 0,
          task = Nothing,
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
                }
            ]
        }
