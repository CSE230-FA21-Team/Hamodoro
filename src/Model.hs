{-# LANGUAGE RecordWildCards #-}

-- Model: Data structures

module Model
  ( Widget (..),
    Panel (..),
    State (..),
    Tick (..),
    Task (..),
    editor,
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

data Widget
  = Default
  deriving (Show, Eq, Ord)

data Panel
  = Editor -- TODO: add sheet later
  deriving (Eq)

data Task = Task
  { title :: String,
    notes :: String,
    duration :: Int,
    startTime :: ZonedTime,
    endTime :: ZonedTime
  }

editor :: Lens' State (E.Editor String Widget)
editor f s = (\x -> s {_editor = x}) <$> f (_editor s)
