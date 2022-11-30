module Test.Unit (unitTests) where

import Plutus.V1.Ledger.Value (singleton)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Utils (returnsFalse, returnsTrue)
import Utils (allWith, oneOf, pconst, ptrue)

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ allWithTest
    , oneOfTest
    ]

allWithTest :: TestTree
allWithTest =
  testGroup
    "allWith"
    [ testCase "trivial single element" $
        let trivialPred = pconst ptrue
            value = pconstant $ singleton "" "" 1
         in returnsTrue (allWith # trivialPred # trivialPred # trivialPred # value)
    , testCase "trivial several elements" $
        let trivialPred = pconst ptrue
            value = pconstant (singleton "aa" "aa" 1 <> singleton "bb" "bb" 2 <> singleton "cc" "cc" 1)
         in returnsTrue (allWith # trivialPred # trivialPred # trivialPred # value)
    ]

oneOfTest :: TestTree
oneOfTest =
  testGroup
    "oneOf"
    [ testCase "single element" $
        let value = pconstant $ singleton "aa" "aa" 1
         in returnsTrue (oneOf # pconstant "aa" # pconstant "aa" # value)
    , testCase "several elements" $
        let value = pconstant $ singleton "aa" "aa" 1 <> singleton "bb" "bb" 1 <> singleton "cc" "cc" 1
         in returnsTrue (oneOf # pconstant "aa" # pconstant "aa" # value)
    , testCase "fails when quantity is > 1" $
        let value = pconstant $ singleton "aa" "aa" 2 <> singleton "bb" "bb" 1 <> singleton "cc" "cc" 1
         in returnsFalse (oneOf # pconstant "aa" # pconstant "aa" # value)
    ]
