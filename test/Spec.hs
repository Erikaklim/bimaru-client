import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y (  encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat )

import Lib3 (parseDocument)
import Lib2 (renderDocument, emptyState, gameStart, hint)
import Lib1 (State(..))
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  fromYamlTests,
  toYamlTests,
  gameStartTests,
  hintTests,
  properties])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (friendlyEncode doc) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [  
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "---\nnull"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "---\n5"
    , testCase "negative int" $
        renderDocument (DInteger (-5)) @?= "---\n-5"
    , testCase "string" $
        renderDocument (DString "string") @?= "---\nstring"
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6, DInteger 7]) @?= listOfInts
    , testCase "list of strings" $
        renderDocument (DList [DString "string1", DString "string2"]) @?= listOfStrings
    , testCase "list of lists" $
        renderDocument (DList [ DString "string1", DList[DInteger 2, DString "string2"] ,DList[DInteger 3, DInteger 4]]) @?= listOfLists
    , testCase "list of lists of lists" $
        renderDocument (DList [DInteger 1, DList[DInteger 8, DList[DString "string", DInteger 5], DInteger 6]]) @?= listOfListsOfLists
    , testCase "list of lists of lists of lists" $
        renderDocument (DList [DInteger 1, DList[DInteger 8, DList[DString "string", DInteger 5, DList [DInteger 7]], DInteger 6]]) @?= listOfListsOfListsOfLists
    , testCase "map" $
        renderDocument (DMap [("key1", DInteger 3), ("key2", DString "string")]) @?= mapYaml
    , testCase "list of maps" $
        renderDocument (DList [DInteger 4, DMap [("key1", DInteger 3), ("key2", DString "string1")] ,DMap [("key3", DInteger 5), ("key4", DString "string2")]]) @?= listOfMaps
    , testCase "map of list" $
        renderDocument (DMap[("key1", DList[DInteger 1, DInteger 2]), ("key2", DInteger 5)]) @?= mapOfList
    , testCase "map of map" $
        renderDocument (DMap[("key1", DMap[("key1.1", DInteger 4), ("key1.2", DList[DInteger 3, DInteger 7])]), ("key2", DString "string")]) @?= mapOfMap
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [   testCase "null" $
        getRight(parseDocument "---\nnull") @?= DNull
    , testCase "int" $
        getRight(parseDocument "---\n5") @?= DInteger 5
    , testCase "negative int" $
        getRight(parseDocument "---\n-5") @?= DInteger (-5)
    , testCase "string" $
        getRight(parseDocument "---\nstring") @?= DString "string"
    , testCase "empty string" $
        getRight(parseDocument "---\n''") @?= DString ""
    , testCase "list of ints" $
        getRight(parseDocument listOfInts) @?= DList [DInteger 5, DInteger 6, DInteger 7]
    , testCase "list of strings" $
        getRight(parseDocument listOfStrings) @?= DList [DString "string1", DString "string2"]
    , testCase "list of lists" $
        getRight(parseDocument listOfLists) @?= DList [ DString "string1", DList[DInteger 2, DString "string2"] ,DList[DInteger 3, DInteger 4]]
    , testCase "list of lists of lists" $
        getRight(parseDocument listOfListsOfLists) @?= DList [DInteger 1, DList[DInteger 8, DList[DString "string", DInteger 5], DInteger 6]]
    , testCase "empty list" $
        getRight(parseDocument "---\n[]") @?= DList []
    , testCase "map" $
        getRight(parseDocument mapYaml) @?= DMap [("key1", DInteger 3), ("key2", DString "string")]
    , testCase "list of maps" $
        getRight(parseDocument listOfMaps) @?= DList [DInteger 4, DMap [("key1", DInteger 3), ("key2", DString "string1")] ,DMap [("key3", DInteger 5), ("key4", DString "string2")]]
    , testCase "map of lists" $
        getRight(parseDocument mapOfList) @?= DMap[("key1", DList[DInteger 1, DInteger 2]), ("key2", DInteger 5)]
    , testCase "map of map" $
        getRight(parseDocument mapOfMap) @?= DMap[("key1", DMap[("key1.1", DInteger 4), ("key1.2", DList[DInteger 3, DInteger 7])]), ("key2", DString "string")]
    , testCase "empty map" $
        getRight(parseDocument "---\n{}") @?= DMap []
   ]

listOfInts :: String
listOfInts = unlines [
      "---"
    , "- 5"
    , "- 6"
    , "- 7"
  ]

listOfStrings :: String
listOfStrings = unlines [
      "---"
    , "- string1"
    , "- string2"
 ]

listOfLists :: String
listOfLists = unlines [
      "---"
    , "- string1"
    , "-"
    , "  - 2"
    , "  - string2"
    , "-"
    , "  - 3"
    , "  - 4"
 ]

listOfListsOfLists :: String
listOfListsOfLists = unlines [
      "---"
    , "- 1"
    , "-"
    , "  - 8"
    , "  -"
    , "    - string"
    , "    - 5"
    , "  - 6"
 ]

listOfListsOfListsOfLists :: String
listOfListsOfListsOfLists = unlines [
      "---"
    , "- 1"
    , "-"
    , "  - 8"
    , "  -"
    , "    - string"
    , "    - 5"
    , "    -"
    , "      - 7"
    , "  - 6"
 ]

mapYaml :: String
mapYaml = unlines [
       "---"
     , "key1: 3"
     , "key2: string"
  ]
 
listOfMaps :: String
listOfMaps = unlines [
      "---"
    , "- 4"
    , "-"
    , "  key1: 3"
    , "  key2: string1"
    , "-"
    , "  key3: 5"
    , "  key4: string2"
 ]

mapOfList :: String
mapOfList = unlines [
      "---"
    , "key1:"
    , "  - 1"
    , "  - 2"
    , "key2: 5"   
 ]

mapOfMap :: String
mapOfMap = unlines [
      "---"
    , "key1:"
    , "  key1.1: 4"
    , "  key1.2:"
    , "    - 3"
    , "    - 7"
    , "key2: string"
 ]


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