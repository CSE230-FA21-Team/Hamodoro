module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Config as C (load)
import Control
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Model
import System.Environment (getArgs)
import Text.Read (readMaybe)
import View

-------------------------------------------------------------------------------
main :: IO ()
main = do
  Right c <- C.load
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  s <- syncFetch c
  res <- customMain initialVty buildVty (Just chan) app (s chan)
  print "exit"

app :: App State Tick Model.Widget
app =
  App
    { appDraw = drawUI,
      appChooseCursor = appCursor,
      appHandleEvent = control,
      appStartEvent = return,
      appAttrMap = const (attrMap defAttr [])
    }
