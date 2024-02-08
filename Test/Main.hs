import Test.HUnit
import System.Exit

test1 :: Test
test1 = TestCase (assertEqual "should return 3" 3 (1 + 2))

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then exitFailure else exitSuccess
