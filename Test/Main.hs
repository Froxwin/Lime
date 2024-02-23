import           Camera
import           Things.Sphere
import           Ray
import           Test.Tasty
import           Test.Tasty.HUnit
import           Vector

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup
  "Unit tests"
  [ testCase "Hitting Sphere"
    $   rayColor (Ray (Vector 0 0 0) (Vector 0 0 1)) [sphere (Vector 0 0 50) 10]
    @?= Color 0.5 0.5 0
  ]
