module Main where

import qualified Brick.AttrMap as A
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Main as M
import Brick.Util (on)
import qualified Brick.Widgets.Edit as E (editAttr, editFocusedAttr)
import qualified Brick.Widgets.List as L (listAttr, listSelectedAttr)
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
import qualified UI.Style as S (active, activeBold, bold, dark, dim, light)
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
  res <- M.customMain initialVty buildVty (Just chan) app (s chan)
  print "exit"

attrMap :: A.AttrMap
attrMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.black),
      (L.listSelectedAttr, V.black `on` V.white),
      (S.bold, V.withStyle V.defAttr V.bold),
      (S.dim, V.withStyle V.defAttr V.dim),
      (S.active, V.magenta `on` V.black),
      ( S.activeBold,
        V.withForeColor
          (V.withBackColor (V.withStyle V.defAttr V.bold) V.black)
          V.magenta
      ),
      (S.light, V.white `on` V.brightBlack),
      (S.dark, V.brightBlack `on` V.black),
      (E.editAttr, V.white `on` V.black),
      (E.editFocusedAttr, V.brightWhite `on` V.black)
    ]

app :: M.App State Tick Model.Widget
app =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = control,
      M.appStartEvent = return,
      M.appAttrMap = const attrMap
    }
