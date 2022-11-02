import Test.Tasty
import Test.Tasty.HUnit

import Lib2 (emptyState, gameStart, emptyState, hint)
import Lib1 (State(..))
--render document pridet kai bus .yaml
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  -- toYamlTests,
  -- fromYamlTests,
  gameStartTests,
  hintTests])

-- fromYamlTests :: TestTree
-- fromYamlTests = testGroup "Document from yaml"
--   [   testCase "null" $
--         parseDocument "null" @?= Right DNull
--     -- IMPLEMENT more test cases:
--     -- * other primitive types/values
--     -- * nested types
--   ]

-- toYamlTests :: TestTree
-- toYamlTests = testGroup "Document to yaml"
--   [   testCase "null" $
--         renderDocument DNull @?= "null"
--     , testCase "int" $
--         renderDocument (DInteger 5) @?= "5"
--     , testCase "string" $
--         renderDocument (DString "string") @?= "string"
--     , testCase "list of ints" $
--         renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
--     ,testCase "list of strings" $
--         renderDocument (DList [DString "string1", DString "string2"]) @? =
--     -- IMPLEMENT more test cases:
--     -- * other primitive types/values
--     -- * nested types
--   ]

-- listOfInts :: String
-- listOfInts = unlines [
--       "---"
--     , "- 5"
--     , "- 6"
--   ]

-- listOfStrings :: String
-- listOfStrings = unlines []

-- map :: String
-- map = unlines []

-- listOfMaps :: String
-- listOfMaps = unlines []

-- listOfLists :: String
-- listOfLists = unlines []

-- listOfListsOfLists :: String
-- listOfStrings = unlines []

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document"
  [
    testCase "makeCords input test 1" $
        getLeft (gameStart emptyState DNull) @?= "Error: Wrong parameters (makeCords)" 
    , testCase "makeCords input test 2" $
        getLeft (gameStart emptyState (DList[DInteger 1])) @?= "Error: Wrong parameters (makeCords)"
    , testCase "makeList document DInteger test 1" $
        getLeft (gameStart emptyState (DMap[("occupied_cols", DList [DString "string"])])) @?= "Error: It's not DInteger"  
    , testCase "makeList document DInteger test 2" $
        getLeft (gameStart emptyState (DMap[("occupied_cols", DList [DInteger 4]),("occupied_rows", DList[DInteger 2, DNull])])) @?= "Error: It's not DInteger"  
    , testCase "makeList input test 1" $
        getLeft (gameStart emptyState (DMap[("occupied_cols", DInteger 1)])) @?= "Error: Wrong parameters (makeList)"
    , testCase  "makeList input test 2" $ 
        getLeft (gameStart emptyState (DMap[("occupied_rows", DList[])])) @?= "Error: Wrong parameters (makeList)"
    , testCase "makeList document string input test 1" $
        getLeft(gameStart emptyState (DMap[("string", DList[DInteger 1])])) @?= "Error: Wrong string" 
    , testCase "makeList document string input test 2" $
        getLeft(gameStart emptyState (DMap[("ocuppied_cols", DList[DInteger 2]), ("string", DList[DInteger 4])])) @?= "Error: Wrong string" 
    , testCase "makeTuple input test 1" $
        getLeft (gameStart emptyState (DMap[("occupied_cols", DList [DInteger 2, DInteger 4]), ("occupied_rows", DList [DInteger 2])])) @?= "Error: Wrong parameters (makeTuple)"
    ,  testCase "makeTuple input test 2" $
    getLeft (gameStart emptyState (DMap[("occupied_cols", DList [DInteger 2]), ("occupied_rows", DList [DInteger 2, DInteger 4])])) @?= "Error: Wrong parameters (makeTuple)"
    , testCase "gameStart no error test" $
        getLeft (gameStart emptyState gameStartDoc) @?= "Error: Didn't get Left"
    , testCase "gameStart successful test 1" $
        getRight (gameStart emptyState gameStartDoc) @?= Cord[(2, 4, 'x')] 
    ,testCase "gameStart successful test 2" $
        getRight (gameStart notEmptyState gameStartDoc) @?= Cord[(2, 4, 'x'), (8, 0, 'x')]
  ]

gameStartDoc :: Document
gameStartDoc = DMap[("number_of_hints", DInteger 10), ("occupied_cols", DList [DInteger 2]), ("occupied_rows", DList [DInteger 4])]

notEmptyState :: State
notEmptyState = Cord[(8, 0, 'x')]


hintTests :: TestTree
hintTests = testGroup "Test hint document"
  [
    testCase "hintf input test 1" $
        getLeft(hint hintState (DInteger 1)) @?= "Error: Wrong parameters"
    , testCase "hintf input test 2" $
        getLeft(hint hintState (DMap[("head", DNull)])) @?= "Error: Wrong parameters"
    , testCase "hintf document string input test 1" $
        getLeft(hint hintState (DMap [("string" ,DMap[])]))  @?= "Error: Wrong string"
    , testCase "hintf document string input test 2" $
        getLeft(hint hintState (DMap[("coords", DMap[("string", DInteger 4)])])) @?= "Error: Wrong string"
    , testCase "hintf document first parameter test 1" $
        getLeft(hint hintState (DMap[("col", DString "hello")])) @?= "Error: Wrong first parameter"
    , testCase "hintf document first parameter test 2" $
        getLeft(hint hintState (DMap[("row", DList[DNull])])) @?= "Error: Wrong first parameter"
    , testCase "hintf document second parameter test 1" $
        getLeft(hint hintState (DMap [("coords", DMap [("head", DInteger 4)])])) @?= "Error: Wrong second parameter"
    , testCase "hintf document second parameter test 2" $
        getLeft(hint hintState (DMap [("coords" ,DMap [("head",DMap [("col", DNull)])])])) @?= "Error: Wrong second parameter"
    , testCase "hint no error test 1" $
        getLeft (hint hintState DNull) @?= "Error: Didn't get Left"
    , testCase "hint no error test 2" $
        getLeft (hint hintState hintDoc) @?= "Error: Didn't get Left"
    , testCase "hint successful test 1" $
        getRight (hint hintState DNull) @?= Cord[(2, 4, 'x')]
    , testCase "hint successful test 2"  $
        getRight (hint hintState hintDoc ) @?= Cord[(8, 0, 'o'), (2, 4, 'x')]
    , testCase "hint succesful test 3" $
        getRight(hint emptyState hintDoc) @?= Cord[(8, 0, 'o')]
  ]

hintDoc :: Document
hintDoc = DMap [("coords" ,DMap [("head",DMap [("col",DInteger 8),("row",DInteger 0)]),("tail",DNull)])]

hintState :: State
hintState = Cord[(2, 4, 'x')]


getLeft :: Either String b -> String
getLeft (Left a) = a
getLeft (Right _) = "Error: Didn't get Left"

getRight :: Either a b -> b
getRight (Right b) = b
getRight (Left _) = error "Error: Expected Right but got Left"