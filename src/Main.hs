module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
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
  rounds <- fromMaybe defaultRounds <$> getRounds
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  res <- customMain initialVty buildVty (Just chan) app (Model.init rounds)
  print (psResult res, psScore res)

app :: App PlayState Tick String
app =
  App
    { appDraw = view,
      appChooseCursor = const . const Nothing,
      appHandleEvent = control,
      appStartEvent = return,
      appAttrMap = const (attrMap defAttr [])
    }

getRounds :: IO (Maybe Int)
getRounds = do
  args <- getArgs
  case args of
    (str : _) -> return (readMaybe str)
    _ -> return Nothing

defaultRounds :: Int
defaultRounds = 3
