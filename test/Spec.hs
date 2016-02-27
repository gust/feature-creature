import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Hspec

import Data.List
import Data.Ord

import qualified Git

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests, hunitSpecs]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" [ ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Testing tests" $
      "hello" @?= "hello"
  ]

hunitSpecs :: TestTree
hunitSpecs = testGroup "Hspec specs" [ ]
