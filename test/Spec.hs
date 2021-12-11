import Lib.Digit
import qualified Config
import Test.Hspec

main :: IO ()
main = do
  testDigits
  testSecondsToDigits
  testOnPause

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
      s <- initState c
      pure $ s {status = Running}
      putStrLn $ "test_status: \n" ++ unlines $ status (onPause s)
      let test_status = status s
      let expect_status = Paused
      test_status `shouldBe` expect_status

testOnClear :: IO ()
testOnClear = hspec $ do
  describe "Testing clearing existing tasks" $ do
    it "should return tasks = [] for onClear state" $ do
      Right c <- Config.load
      s <- initState c
      putStrLn $ "test_tasks: \n" ++ unlines $ tasks (onClear s)
      let test_tasks = tasks s
      let expect_tasks = []
      test_tasks `shouldBe` expect_tasks

testOnRestart :: IO ()
testOnRestart = hspec $ do
  describe "Testing restarting new sessions" $ do
    it "should return status = Ready for onRestart state" $ do
      Right c <- Config.load
      s <- initState c
      pure $ s {status = Running}
      putStrLn $ "test_status: \n" ++ unlines $ status (onRestart s)
      let test_status = status s
      let expect_status = Ready
      test_status `shouldBe` expect_status
    it "should return _panel = Editor for onRestart state" $ do
      Right c <- Config.load
      s <- initState c
      pure $ s {_panel = Schedule}
      putStrLn $ "test_panel: \n" ++ unlines $ _panel (onRestart s)
      let test_panel = _panel s
      let expect_panel = Editor
      test_panel `shouldBe` expect_panel