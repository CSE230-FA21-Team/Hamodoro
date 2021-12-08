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
import Data.List (find, intercalate)
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
import qualified Brick.Types as T (BrickEvent (..), EventM, Next, handleEventLensed)
import qualified Brick.Widgets.Edit as E
  ( applyEdit,
    editor,
    getEditContents,
    handleEditorEvent,
  )
import Lens.Micro
import Config (Config)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time.Clock (DiffTime, UTCTime, diffUTCTime, getCurrentTime, utctDay)
import Data.Time.LocalTime (ZonedTime (..), getZonedTime)
import qualified Graphics.Vty as V
import Data.Time.LocalTime (ZonedTime (..), getZonedTime)
import qualified Brick.Focus as F
import Lib
import Model
  ( Panel (Editor),
    State (..),
    Task (..),
    Tick (..),
    Widget (..),
    Name (..),
    editor1,
    editor2,
    editor3
  )

-- import Model.Player

-------------------------------------------------------------------------------

control :: State -> T.BrickEvent Name Tick -> EventM Name (T.Next State)
control s@State {panel = p} (T.VtyEvent ev) = 
  case (p, ev) of
  --   AppEvent Tick -> nextS s =<< liftIO (play O s)
  -- AppEvent Tick -> M.continue =<< liftIO (autoRefresh s)
  -- AppEvent s (T.AppEvent Tick) = M.continue =<< liftIO (autoRefresh s)
    (Editor, V.EvKey V.KEsc _) -> M.halt s
    (Editor, V.EvKey (V.KChar '\t') _) -> M.continue (s {_focusRing = F.focusNext (_focusRing s)})
    (Editor, V.EvKey V.KEnter _) -> M.continue =<< liftIO (save s)
    (Editor, _) -> M.continue =<< case F.focusGetCurrent (_focusRing s) of
               Just Edit1 -> T.handleEventLensed s editor1 E.handleEditorEvent ev
               Just Edit2 -> T.handleEventLensed s editor2 E.handleEditorEvent ev
               Just Edit3 -> T.handleEventLensed s editor3 E.handleEditorEvent ev
               Nothing -> return s
control s _ = M.continue s -- Brick.halt s

save :: State -> IO State
save s@State {tasks = ts, _editor1 = ed1, _editor2 = ed2, _editor3 = ed3} = do 
  pure $ s {tasks = ts ++ [t]}
  where title1 = intercalate "; " . filter (/= "") $ E.getEditContents ed1
        notes1 = intercalate "; " . filter (/= "") $ E.getEditContents ed2 
        duration1 = (E.getEditContents ed3) !! 0 
        t = Task {
          title = title1,
          notes = notes1,
          duration = read duration1
        }
    --startTime = zoneT,
    --endTime = zoneT}



autoRefresh :: State -> IO State
autoRefresh s = do
  -- d <- getCurrentTime
  -- if diffUTCTime d (_now s) > 30
  --   then refresh s
  --   else pure s
  pure s

syncFetch :: Config -> IO (BChan Tick -> State)
syncFetch c = do
  d <- getCurrentTime
  zoneT <- getZonedTime
  pure $ \q ->
    State
      { config = c,
        panel = Editor,
        _editor1 = E.editor Edit1 Nothing "",
        _editor2 = E.editor Edit2 Nothing "",
        _editor3 = E.editor Edit3 Nothing "0",
        _focusRing = F.focusRing [Edit1, Edit2, Edit3],
        now = d,
        day = (utctDay d),
        -- TODO: change tasks back to []
        tasks =
          [ Task
              { title = "test task 1",
                notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                duration = 20
                --startTime = zoneT,
                --endTime = zoneT
              }
            --Task
            --  { title = "test task 2",
            --    notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque e",
            --    duration = "40"
                --startTime = zoneT,
                --endTime = zoneT
            --  }
          ]
      }

renderNotes :: String -> String
renderNotes = unlines . filter (/= "") . map trimLeft . splitOn ';'
