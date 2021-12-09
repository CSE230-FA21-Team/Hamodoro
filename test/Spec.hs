import Lib.Digit
import Test.Hspec

main :: IO ()
main = do
  testDigits

testDigits :: IO ()
testDigits = hspec $ do
  describe "Testing time to digital font" $ do
    it "should return the correct string list for 12:34" $ do
      let digits_str = timeToDigitalLines [1, 2, 3, 4]
      let expect_str = [".....#.######...######.#......", ".....#......#.#......#.#......", ".....#.######......###.#...#..", ".....#.#......#......#.######.", ".....#.######...######.....#.."]
      putStrLn $ "digits_str: \n" ++ (unlines digits_str)
      digits_str `shouldBe` expect_str
    it "should return the correct string list for 95:27" $ do
      let digits_str = timeToDigitalLines [9, 5, 2, 7]
      let expect_str = ["######.######...######......#.", "#....#.#....#.#......#......#.", "######.######...######......#.", ".....#......#.#.#...........#.", "######.######...######......#."]
      putStrLn $ "digits_str: \n" ++ (unlines digits_str)
      digits_str `shouldBe` expect_str
