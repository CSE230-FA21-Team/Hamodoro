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
import Config (Config)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time.Clock (DiffTime, UTCTime, diffUTCTime, getCurrentTime, utctDay)
import qualified Graphics.Vty as V
import Data.Time.LocalTime (ZonedTime (..), getZonedTime)
import Lib
import Model 
-- import qualified UI.Editor as Editor (render)

-- import Model.Player

-------------------------------------------------------------------------------

control :: State -> T.BrickEvent Model.Widget Tick -> EventM Model.Widget (T.Next State)
control s@State {panel = p} (T.VtyEvent ev) = 
  case (p, ev) of
  --   AppEvent Tick -> nextS s =<< liftIO (play O s)
  -- AppEvent Tick -> M.continue =<< liftIO (autoRefresh s)
  -- AppEvent s (T.AppEvent Tick) = M.continue =<< liftIO (autoRefresh s)
    (Editor, V.EvKey V.KEsc _) -> M.halt s
    (Editor, V.EvKey V.KEnter _) -> M.continue =<< liftIO (save s)
    (Editor, _) -> M.continue =<< edit s ev
control s _ = M.continue s -- Brick.halt s

edit :: State -> V.Event -> T.EventM Model.Widget State
edit s = T.handleEventLensed s editor E.handleEditorEvent

save :: State -> IO State
save s@State {tasks = ts, _editor = ed} = do 
  pure $ s {tasks = ts ++ [t]}
  where title1 = intercalate "; " . filter (/= "") $ E.getEditContents ed 
        t = Task {
          title = title1,
          notes = "",
          duration = 0
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
  pure $ \q ->
    State
      { config = c,
        panel = Editor,
        _editor = E.editor Default Nothing (renderNotes $ ""),
        now = d,
        day = undefined,
        tasks = []
      }

renderNotes :: String -> String
renderNotes = unlines . filter (/= "") . map trimLeft . splitOn ';'
