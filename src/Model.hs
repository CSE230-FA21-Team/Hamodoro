{-# LANGUAGE RecordWildCards #-}

module Model
  ( Widget (..),
    Panel (..),
    State (..),
    Tick (..),
    Task (..),
    editor,
    syncFetch,
  )
where

import Brick.BChan (BChan, writeBChan)
import qualified Brick.Widgets.Edit as E
  ( Editor,
    applyEdit,
    editor,
    getEditContents,
    handleEditorEvent,
  )
import Config (Config)
import Data.Char (isSpace)
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, UTCTime, diffUTCTime, getCurrentTime, utctDay)
import Data.Time.LocalTime (ZonedTime (..), getZonedTime)
import Lens.Micro (Lens', each, filtered, (%~))
import Prelude hiding ((!!))

-------------------------------------------------------------------------------

-- | Ticks mark passing of time: a custom event that we constantly stream

-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------

-- | Top-level App State ------------------------------------------------------

-------------------------------------------------------------------------------

data State = State
  { -- TODO:
    config :: Config,
    panel :: Panel,
    _editor :: E.Editor String Widget,
    now :: UTCTime,
    day :: Day,
    tasks :: [Task]
  }

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

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s =
  case dropWhile (== c) s of
    [] -> []
    s' -> w : splitOn c s''
      where
        (w, s'') = break (== c) s'

trimLeft :: String -> String
trimLeft = dropWhile isSpace

data Widget
  = Default
  deriving (Show, Eq, Ord)

data Panel
  = Editor -- TODO: add sheet later
  deriving (Eq)

data Task = Task
  { title :: String,
    notes :: String,
    duration :: Float,
    startTime :: ZonedTime,
    endTime :: ZonedTime
  }

editor :: Lens' State (E.Editor String Widget)
editor f s = (\x -> s {_editor = x}) <$> f (_editor s)
