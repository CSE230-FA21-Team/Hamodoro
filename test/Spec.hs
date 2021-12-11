import Lib.Digit
import Config
import Test.Hspec
import Model
import Control
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import Data.Time.Clock
import Data.Time.LocalTime

main :: IO ()
main = do
  testDigits
  testSecondsToDigits
  testOnPause
  testOnClear
  testOnRestart

testDigits :: IO ()
testDigits = hspec $ do
  describe "Testing time to digital font" $ do
    it "should return the correct string list for 12:34" $ do
      let digits_str = timeToDigitalLines [1, 2, 3, 4]
      let expect_str = [".....#.######...######.#......", ".....#......#.#......#.#......", ".....#.######......###.#...#..", ".....#.#......#......#.######.", ".....#.######...######.....#.."]
      putStrLn $ "digits_str: \n" ++ unlines digits_str
      digits_str `shouldBe` expect_str
    it "should return the correct string list for 95:27" $ do
      let digits_str = timeToDigitalLines [9, 5, 2, 7]
      let expect_str = ["######.######...######.######.", "#....#.#......#......#......#.", "######.######...######......#.", ".....#......#.#.#...........#.", "######.######...######......#."]
      putStrLn $ "digits_str: \n" ++ unlines digits_str
      digits_str `shouldBe` expect_str

testSecondsToDigits :: IO ()
testSecondsToDigits = hspec $ do
  describe "Testing seconds to digits conversion" $ do
    it "should return 0,0,0,5 for 5" $ do
      let digits = secondsToDigitArray 5
      digits `shouldBe` [0, 0, 0, 5]
    it "should print correct number for 5" $ do
      putStrLn $ "digits_str: \n" ++ unlines (secondsToDigitLines 5)

testOnPause :: IO ()
testOnPause = hspec $ do
  describe "Testing pausing timer" $ do
    it "should return status = paused for onPause state" $ do
      Right c <- Config.load
      d <- getCurrentTime
      z <- getZonedTime
      let s = State { 
          config = c,
          _panel = Editor,
          status = Running,
          _editor1 = E.editor Edit1 (Just 1) "",
          _editor2 = E.editor Edit2 Nothing "",
          _editor3 = E.editor Edit3 (Just 1) "0",
          _focusRing = F.focusRing [Edit1, Edit2, Edit3],
          notification = " ",
          now = d,
          day = utctDay d,
          countdown = 0,
          task = Nothing,
          tasks = [
            Task
                { title = "test task 1",
                  notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                  duration = 20,
                  startTime = z,
                  endTime = z
                }
          ]
      }
      pure $ onPause s
      if status s == Paused then
        putStrLn $ "test_status: \n" ++ "Paused"
      else
        putStrLn $ "failed \n"
      let test_status = status s
      --let expect_status = Paused
      test_status `shouldBe` Paused

testOnClear :: IO ()
testOnClear = hspec $ do
  describe "Testing clearing existing tasks" $ do
    it "should return tasks = [] for onClear state" $ do
      Right c <- Config.load
      d <- getCurrentTime
      z <- getZonedTime
      let s = State { 
          config = c,
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
          tasks = [
            Task
                { title = "test task 1",
                  notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                  duration = 20,
                  startTime = z,
                  endTime = z
                }
            ]
          }
      pure $ onClear s
      if length (tasks s) == 0 then
        putStrLn $ "test_tasks: \n" ++ "[]"
      else
        putStrLn $ "failed \n"
      let test_tasks = tasks s
      let expect_tasks = []
      length test_tasks `shouldBe` 0

testOnRestart :: IO ()
testOnRestart = hspec $ do
  describe "Testing restarting new sessions" $ do
    it "should return status = Ready for onRestart state" $ do
      d <- getCurrentTime
      z <- getZonedTime
      Right c <- Config.load
      let s = State { 
          config = c,
          _panel = Editor,
          status = Running,
          _editor1 = E.editor Edit1 (Just 1) "",
          _editor2 = E.editor Edit2 Nothing "",
          _editor3 = E.editor Edit3 (Just 1) "0",
          _focusRing = F.focusRing [Edit1, Edit2, Edit3],
          notification = " ",
          now = d,
          day = utctDay d,
          countdown = 0,
          task = Nothing,
          tasks = [
            Task
                { title = "test task 1",
                  notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                  duration = 20,
                  startTime = z,
                  endTime = z
                }
          ]
      }
      pure $ onRestart s
      if status s == Ready then
        putStrLn $ "test_status: \n" ++ "Ready"
      else
        putStrLn $ "failed \n"
      let test_status = status s
      let expect_status = Ready
      test_status `shouldBe` expect_status
    it "should return _panel = Editor for onRestart state" $ do
      d <- getCurrentTime
      z <- getZonedTime
      Right c <- Config.load
      let s = State { 
          config = c,
          _panel = Schedule,
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
          tasks = [
            Task
                { title = "test task 1",
                  notes = "Lorem ipsum dolor sit amet, ubique neglegentur eu mel, dicat aeque evertitur mei id.",
                  duration = 20,
                  startTime = z,
                  endTime = z
                }
          ]
      }
      pure $ onRestart s
      if _panel s == Editor then
        putStrLn $ "test_panel: \n" ++ "Editor"
      else
        putStrLn $ "failed \n"
      let test_panel = _panel s
      let expect_panel = Editor
      test_panel `shouldBe` expect_panel