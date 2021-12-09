{-# LANGUAGE RecordWildCards #-}

-- Model: Data structures

module Model
  ( Widget (..),
    Panel (..),
    State (..),
    Tick (..),
    Task (..),
    Status (..),
    editor1,
    editor2,
    editor3,
    panel,
  )
where

import Brick.BChan (BChan, writeBChan)
import qualified Brick.Focus as F
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
import Lens.Micro
--import Lens.Micro.TH
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
    _panel :: Panel,
    status :: Status,
    _editor1 :: E.Editor String Widget,
    _editor2 :: E.Editor String Widget,
    _editor3 :: E.Editor String Widget,
    _focusRing :: F.FocusRing Widget,
    now :: UTCTime,
    countdown :: Int,
    day :: Day,
    tasks :: [Task]
  }

data Widget
  = Default
  | Edit1
  | Edit2
  | Edit3
  deriving (Show, Eq, Ord)

data Panel
  = Editor -- TODO: add sheet later
  | Ending
  | Schedule
  deriving (Eq)

data Status
  = Ready
  | Running
  | Paused
  | Finished
  deriving (Eq)

data Task = Task
  { title :: String,
    notes :: String,
    -- TODO
    duration :: Int,
    startTime :: ZonedTime,
    endTime :: ZonedTime
  }

--makeLenses ''State
editor1 :: Lens' State (E.Editor String Widget)
editor1 f s = (\x -> s {_editor1 = x}) <$> f (_editor1 s)

editor2 :: Lens' State (E.Editor String Widget)
editor2 f s = (\x -> s {_editor2 = x}) <$> f (_editor2 s)

editor3 :: Lens' State (E.Editor String Widget)
editor3 f s = (\x -> s {_editor3 = x}) <$> f (_editor3 s)

panel :: Lens' State Panel
panel f s = (\x -> s {_panel = x}) <$> f (_panel s)
