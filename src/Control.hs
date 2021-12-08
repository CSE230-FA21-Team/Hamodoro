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
import qualified Brick.Types as T (BrickEvent (..), EventM, handleEventLensed)
import qualified Brick.Widgets.Edit as E
  ( applyEdit,
    editor,
    getEditContents,
    handleEditorEvent,
  )
import Config (Config)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time.Clock (DiffTime, UTCTime, diffUTCTime, getCurrentTime, utctDay)
import Data.Time.LocalTime (ZonedTime(..), getZonedTime)
import qualified Graphics.Vty as V
import Lib
import Model
    ( Task(..),
      Panel(Editor),
      Widget(..),
      State(..),
      Tick(..),
      editor )

-- import Model.Player

-------------------------------------------------------------------------------

control :: State -> BrickEvent n Tick -> EventM n (Next State)
control s ev = case ev of
  --   AppEvent Tick -> nextS s =<< liftIO (play O s)
  AppEvent Tick -> M.continue =<< liftIO (autoRefresh s)
  -- AppEvent s (T.AppEvent Tick) = M.continue =<< liftIO (autoRefresh s)
  T.VtyEvent (V.EvKey V.KEsc _) -> Brick.halt s
  _ -> Brick.continue s -- Brick.halt s

edit :: State -> V.Event -> T.EventM Model.Widget State
edit s = T.handleEventLensed s editor E.handleEditorEvent

--  save :: State -> IO State

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
        _editor = E.editor Default Nothing (renderNotes $ ""),
        now = d,
        day = (utctDay d),
        -- TODO: change tasks back to []
        tasks = [
          Task {
            title = "test task 1", 
            notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
            duration = 20,
            startTime = zoneT,
            endTime = zoneT
          },
          Task {
            title = "test task 2", 
            notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque e",
            duration = 20,
            startTime = zoneT,
            endTime = zoneT
          }
        ]
      }

renderNotes :: String -> String
renderNotes = unlines . filter (/= "") . map trimLeft . splitOn ';'
