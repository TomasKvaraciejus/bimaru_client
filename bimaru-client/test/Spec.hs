import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)

import Lib1(State(..))
import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument)
import Types (Document(..), Coord(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  properties])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

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

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [ 
    testCase "null" $
          parseDocument "null\n" @?= Right DNull
    , testCase "Empty DMap" $
        parseDocument "{}\n" @?= Right (DMap [])
    , testCase "Empty DList" $
        parseDocument "[]\n" @?= Right (DList  [])
    , testCase "DMap in DList" $
        parseDocument "- AAA: BBB\n" @?= Right (DList  [DMap [("AAA", DString "BBB")]])
    , testCase "Empty DList in DMap" $
        parseDocument "AAA:\n  []\n" @?= Right (DMap [("AAA", DList [])])
    , testCase "Empty DList in DList" $
        parseDocument "- []\n" @?= Right (DList [DList []])
    , testCase "Empty DMap in DMap" $
        parseDocument "AAA: {}\n" @?= Right (DMap [("AAA", DMap [])])
    , testCase "DMap in DMap" $
        parseDocument "AAA:\n  BBB: ABC\n" @?= Right (DMap [("AAA", DMap [("BBB", DString "ABC")])])
    , testCase "DList with 4 elements" $
        parseDocument "- a\n- b\n- c\n- d\n" @?= Right (DList [DString "a", DString "b", DString "c", DString "d"])
    , testCase "Negative DInteger" $
        parseDocument "-3\n" @?= Right (DInteger (-3))
    , testCase "DInteger in list" $
        parseDocument "- 3\n" @?= Right (DList [DInteger 3])
    , testCase "Negative DInteger in list" $
        parseDocument "- -3\n" @?= Right (DList [DInteger (-3)])
    , testCase "DString with number" $
        parseDocument "'3'\n" @?= Right (DString "3")
    , testCase "DString with spacing" $
        parseDocument "'aa bb'\n" @?= Right (DString "aa bb")
    , testCase "Multiple DMaps on same level" $
        parseDocument "aaa: bbb\nABA: 3\n" @?= Right (DMap [("aaa", DString "bbb"), ("ABA", DInteger 3)])
    , testCase "Multiple DMaps on same nested level" $
        parseDocument "abc:\n  aaa: bbb\n  ABA: 3\n" @?= Right (DMap [("abc", DMap [("aaa", DString "bbb"), ("ABA", DInteger 3)])])
    , testCase "Multiple Lists on same nested level" $
        parseDocument "- - aaa\n  - bbb\n  - abc\n" @?= Right (DList [DList [DString "aaa", DString "bbb", DString "abc"]])
    , testCase "DMap value missing spacing" $
        parseDocument "aaa:\nbbb\n" @?= Left "incorrect height formatting. In expression -> bbb\n"
    , testCase "Auto-generated test 1" $
        parseDocument "ZovsapRAp:\n- -4\n- - bHV:\n      eQYVrpf: {}\n    chUnbzBf: 6\n    wnstZFdus:\n      sCRItxD: zP76\n  - []\n  - []\n- 4\n- - {}\n" @?= Right (DMap [("ZovsapRAp",DList [DInteger (-4),DList [DMap [("bHV",DMap [("eQYVrpf",DMap [])]),("chUnbzBf",DInteger 6),("wnstZFdus",DMap [("sCRItxD",DString "zP76")])],DList [],DList []],DInteger 4,DList [DMap []]])])
    , testCase "Auto-generated test 2" $
        parseDocument "- zRx:\n    BmscW:\n    - 9\n    - 11\n    - '7  '\n    p: {}\n  Cc:\n  - 4\n  - -8\n  - HVO 38z 7z\n  - m   tdM\n  ImttAwtvon: -12\n- -1\n- 11\n" @?= Right (DList [DMap [("Cc",DList [DInteger 4,DInteger (-8),DString "HVO 38z 7z",DString "m   tdM"]),("ImttAwtvon",DInteger (-12)),("zRx",DMap [("BmscW",DList [DInteger 9,DInteger 11,DString "7  "]),("p",DMap [])])],DInteger (-1),DInteger 11])
    , testCase "Auto-generated test 3" $
        parseDocument "EkhDQYjrZI: ' 3x QPQ ZW'\ne: 3\nIwpBLDqSj:\n- 6x u51 MB\n- - -4\n  - z: Cd t\n    bciXGwxY: RcaT\n    dFz:\n      Hd:\n      - 7c Sw\n      - {}\n      - - 12\n      TsmhmC: ' s'\n      NSMZbK:\n      - '09 WV5l  H '\n      - {}\n  - []\nUuunKin: Ni    8mIPS\n" @?= Right (DMap [("EkhDQYjrZI",DString " 3x QPQ ZW"),("IwpBLDqSj",DList [DString "6x u51 MB",DList [DInteger (-4),DMap [("bciXGwxY",DString "RcaT"),("dFz",DMap [("Hd",DList [DString "7c Sw",DMap [],DList [DInteger 12]]),("NSMZbK",DList [DString "09 WV5l  H ",DMap []]),("TsmhmC",DString " s")]),("z",DString "Cd t")],DList []]]),("UuunKin",DString "Ni    8mIPS"),("e",DInteger 3)])
    , testCase "DMap missing space" $
        parseDocument "aaa:a\n" @?= Left "No space after DMap key. In expression -> aaa:a"
    , testCase "DMap inconsistent spacing" $
        parseDocument "aaa:\n a\nbbb:\n    b\nABA:\n  aaa\n" @?= Right (DMap [("aaa", DString "a"), ("bbb", DString "b"), ("ABA", DString "aaa")])
  ]

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null\n"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5\n"
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    , testCase "Big boi" $
        renderDocument (DMap[("game_setup_id", DString "0de28b51-e8ef-41d5-a1e6-131b51c4a638"), ("number_of_hints", DInteger 10), ("occupied_rows", DMap[("head", DInteger 3), ("tail", DMap[("head", DInteger 3), ("tail", DMap[("head", DInteger 0), ("tail", DMap[("head", DInteger 0), ("tail", DMap[("head", DInteger 3), ("tail", DMap[("head", DInteger 0), ("tail", DMap[("head", DInteger 5), ("tail", DMap[("head", DInteger 0), ("tail", DMap[("head", DInteger 4), ("tail", DMap[("head", DInteger 2), ("tail", DNull)])])])])])])])])])]), ("occupied_cols", DMap[("head", DInteger 1), ("tail", DMap[("head", DInteger 1), ("tail", DMap[("head", DInteger 4), ("tail", DMap[("head", DInteger 2), ("tail", DMap[("head", DInteger 2), ("tail", DMap[("head", DInteger 2), ("tail", DMap[("head", DInteger 2), ("tail", DMap[("head", DInteger 2), ("tail", DMap[("head", DInteger 0), ("tail", DMap[("head", DInteger 4), ("tail", DNull)])])])])])])])])])])]) @?=
          "game_setup_id: 0de28b51-e8ef-41d5-a1e6-131b51c4a638\nnumber_of_hints: 10\noccupied_rows:\n  head: 3\n  tail:\n    head: 3\n    tail:\n      head: 0\n      tail:\n        head: 0\n        tail:\n          head: 3\n          tail:\n            head: 0\n            tail:\n              head: 5\n              tail:\n                head: 0\n                tail:\n                  head: 4\n                  tail:\n                    head: 2\n                    tail: null\noccupied_cols:\n  head: 1\n  tail:\n    head: 1\n    tail:\n      head: 4\n      tail:\n        head: 2\n        tail:\n          head: 2\n          tail:\n            head: 2\n            tail:\n              head: 2\n              tail:\n                head: 2\n                tail:\n                  head: 0\n                  tail:\n                    head: 4\n                    tail: null\n"
    , testCase "Dlist" $
        renderDocument (DMap[("John", DList[DString "Hello", DList[DInteger 1, DInteger 2, DMap[("kello", DNull), ("priviet", DInteger 5)], DString "t", DString "q"], DMap[("Hu", DNull), ("John", DInteger 1), ("hi", DString "hello")]]), ("game", DString "o")]) @?=
          "John:\n- Hello\n- - 1\n  - 2\n  - kello: null\n    priviet: 5\n  - t\n  - q\n- Hu: null\n  John: 1\n  hi: hello\ngame: o\n"
    , testCase "DList in DList" $
        renderDocument (DMap[("John", DMap[("Hello", DList[ DString "Nice", DList[DInteger 1, DInteger 2, DMap[("kello", DNull), ("priviet", DInteger 5), ("not", DString "t"), ("map", DMap[("Hu", DMap[("John", DInteger 1), ("yes", DNull)])])], DInteger 5]]), ("game", DString "o")])]) @?=
          "John:\n  Hello:\n  - Nice\n  - - 1\n    - 2\n    - kello: null\n      priviet: 5\n      not: t\n      map:\n        Hu:\n          John: 1\n          'yes': null\n    - 5\n  game: o\n"
    , testCase "DList in DMap" $
        renderDocument (DList[DInteger 4, DInteger 9, DMap[("u", DMap[("red", DList[DMap[("Hello", DInteger 9)], DMap[("green", DInteger 6), ("brown", DList[DMap[("not", DInteger 5)], DMap[("notnice", DInteger 6)]]), ("nice", DString "nice")]])])], DNull]) @?=
          "- 4\n- 9\n- u:\n    red:\n    - Hello: 9\n    - green: 6\n      brown:\n      - not: 5\n      - notnice: 6\n      nice: nice\n- null\n"
    , testCase "DInteger" $
        renderDocument (DInteger 5) @?= "5\n"   
    , testCase "DNull" $
        renderDocument DNull @?= "null\n"
    , testCase "DString" $
        renderDocument (DString "hello") @?= "hello\n"
    , testCase "DMap" $
        renderDocument (DMap[("Hello", DInteger 3)]) @?= "Hello: 3\n"
    , testCase "DMap 2" $
        renderDocument (DMap[("Hello", DInteger 3), ("Hello", DInteger 4)]) @?= "Hello: 3\nHello: 4\n"
    , testCase "DMap nested DMap" $
        renderDocument (DMap[("Hello", DMap[("Bye", DString "red bird")])]) @?= "Hello:\n  Bye: red bird\n"
    , testCase "DMap nested DList" $
        renderDocument (DMap[("Hello", DList[DInteger 4, DString "good", DMap[("Wow", DNull)]])]) @?= "Hello:\n- 4\n- good\n- Wow: null\n"
    , testCase "DList" $
        renderDocument (DList[DInteger 1, DNull, DString "hello"]) @?= "- 1\n- null\n- hello\n"
    , testCase "DList nested DList" $
        renderDocument (DList[DNull, DString "red", DList[DInteger 4, DInteger 0, DMap[("Hello", DString "goodbye")]]]) @?= "- null\n- red\n- - 4\n  - 0\n  - Hello: goodbye\n"
    , testCase "DList nested DList 2" $
        renderDocument (DList[DList[DList[DList[DString "a"]]]]) @?= "- - - - a\n"
    , testCase "DList nested DList 3" $
        renderDocument (DList[DString "aa", DList[DList[DString "aaa", DList[DString "a"]]]]) @?= "- aa\n- - - aaa\n    - - a\n"
    , testCase "Coord List" $
        renderDocument (DMap[("coords", DList [DMap[("col: ", DInteger 1), ("row: ", DInteger 1)]])]) @?= "coords:\n- 'col: ': 1\n  'row: ': 1\n"
  ]

wrongState :: String
wrongState = "Wrong State starting data"

ending :: String
ending = " key not found or is placed incorrectly"

emptyState :: State
emptyState = State 0 [] [] []

listOfInts :: String
listOfInts = unlines [
     "- 5"
    , "- 6"
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" 
  [
    testCase "hints != 0" $
        gameStart (State (-1) [] [] []) DNull @?= Left wrongState
    , testCase "rows != []" $
        gameStart (State 0 [1] [] []) DNull @?= Left wrongState
    , testCase "cols != []" $
        gameStart (State 0 [] [1] []) DNull @?= Left wrongState
    , testCase "toggle != []" $
        gameStart (State 0 [] [] [Coord 1 2]) DNull @?= Left wrongState
    , testCase "Wrong Document: DNull" $
        gameStart emptyState DNull @?= Left wrongState
    , testCase "Wrong Document: DList" $
        gameStart emptyState (DList []) @?= Left wrongState
    , testCase "Wrong Document: DInteger" $
        gameStart emptyState (DInteger 1) @?= Left wrongState
    , testCase "Wrong Document: DMap []" $
        gameStart emptyState (DMap []) @?= Left wrongState
    , testCase "No key number_of_hints" $
        gameStart emptyState (DMap [("hello", DNull)]) @?= Left ("number_of_hints" ++ ending)
    , testCase "No key occupied_rows" $
        gameStart emptyState (DMap [("number_of_hints", DNull), ("hello", DNull)]) @?= Left ("occupied_rows" ++ ending)
    , testCase "No key occupied_cols" $
        gameStart emptyState (DMap [("number_of_hints", DNull), ("occupied_rows", DNull), ("hello", DNull)]) @?= Left ("occupied_cols" ++ ending)
    , testCase "DMap has too big of an integer" $
        gameStart emptyState (DMap [("number_of_hints", DInteger 10), ("occupied_rows", DMap[("head", DInteger 10), ("tail", DNull)]), ("occupied_cols", DNull)]) @?= Left "Found wrong value when checking DMap"
    , testCase "DNull as initial states for occupied_row and occupied_cols" $
        gameStart emptyState (DMap [("number_of_hints", DInteger 1), ("occupied_rows", DNull), ("occupied_cols", DNull)]) @?= Left "improper type when creating a list"
    , testCase "Two tails in a row" $
        gameStart emptyState (DMap [("number_of_hints", DInteger 9), ("occupied_rows", DMap[("tail", DInteger 1)]), ("occupied_cols", DNull)]) @?= Left "Found wrong value when checking DMap"
    , testCase "Two head in a row" $
        gameStart emptyState (DMap [("number_of_hints", DInteger 9), ("occupied_rows", DMap[("head", DInteger 1), ("head", DNull)]), ("occupied_cols", DNull)]) @?= Left "Found wrong value when checking DMap"
    , testCase "Bad second value of first element's first tuple" $
        gameStart emptyState (DMap [("number_of_hints", DInteger 9), ("occupied_rows", DMap[("head", DList[]), ("tail", DNull)]), ("occupied_cols", DNull)]) @?= Left "Found wrong value when checking DMap"
    , testCase "Bad key for second element's key" $
        gameStart emptyState (DMap [("number_of_hints", DInteger 9), ("occupied_rows", DMap[("head", DInteger 4), ("tail", DMap[("h", DInteger 1)])]), ("occupied_cols", DNull)]) @?= Left "Found wrong value when checking DMap"
    , testCase "More bad nested keys in DMap" $
        gameStart emptyState (DMap [("number_of_hints", DInteger 9), ("occupied_rows", DMap[("head", DInteger 1), ("tail", DMap[("h", DList[DInteger 5, DNull])])]), ("occupied_cols", DNull)]) @?= Left "Found wrong value when checking DMap"
    , testCase "More bad nested keys in DMap #2" $
        gameStart emptyState (DMap [("number_of_hints", DInteger 9), ("occupied_rows", DMap[("head", DInteger 1), ("tail", DMap[("head", DInteger 2), ("tail", DString "p")])]), ("occupied_cols", DNull)]) @?= Left "Found wrong value when checking DMap"
    , testCase "Correct Input" $
        gameStart emptyState (DMap [("number_of_hints", DInteger 10), ("occupied_rows", DMap[("head", DInteger 1), ("tail", DMap[("head", DInteger 2), ("tail", DNull)])]), ("occupied_cols", DMap[("head", DInteger 1), ("tail", DMap[("head", DInteger 3), ("tail", DNull)])])]) @?= Right (State 10 [1,2] [1,3] [])
    --analogous tests with "occupied_cols"
  ]

wrongHintState :: String
wrongHintState = "Wrong State Hint data"

hintTests :: TestTree
hintTests = testGroup "Test hint document"
  [
    testCase "hints != 0" $
        hint (State (-1) [] [] []) DNull @?= Left wrongHintState
    , testCase "rows != []" $
        hint (State 0 [1] [] []) DNull @?= Left wrongHintState
    , testCase "cols != []" $
        hint (State 0 [] [1] []) DNull @?= Left wrongHintState
    , testCase "toggle != []" $
        hint (State 0 [] [] [Coord 1 2]) DNull @?= Left wrongHintState
    , testCase "Wrong Document: DNull" $
        hint emptyState DNull @?= Left wrongHintState
    , testCase "Wrong Document: DList" $
        hint emptyState (DList []) @?= Left wrongHintState
    , testCase "Wrong Document: DInteger" $
        hint emptyState (DInteger 1) @?= Left wrongHintState
    , testCase "Wrong Document: DMap []" $
        hint emptyState (DMap []) @?= Left wrongHintState
    , testCase "Wrong first key" $
        hint emptyState (DMap [("john", DList[])]) @?= Left "Tuple must be made out of \"coords\" key and a DList value"
    , testCase "Wrong first value" $
        hint emptyState (DMap [("coords", DNull)]) @?= Left "Tuple must be made out of \"coords\" key and a DList value"
    , testCase "Empty first value" $
        hint emptyState (DMap [("coords", DList[])]) @?= Left "Hint coordinate list must not be empty"
    , testCase "Wrong first's value values" $
        hint emptyState (DMap [("coords", DList[DInteger 1])]) @?= Left "DList must contain only DMaps with 2 tuples each"
    , testCase "Only one tuple" $
        hint emptyState (DMap [("coords", DList[DMap[("john", DInteger 4)]])]) @?= Left "Not enough values in one of Hint's DMaps"
    , testCase "Wrong tuple's key" $
        hint emptyState (DMap [("coords", DList[DMap[("john", DInteger 4), ("row", DInteger 1)]])]) @?= Left "Bad value in one on Hint's DMap values"
    , testCase "Wrong tuple's value" $
        hint emptyState (DMap [("coords", DList[DMap[("col", DMap[]), ("row", DInteger 1)]])]) @?= Left "Tuple most contain a string key and a DInteger value"
    , testCase "Wrong 2nd tuple's key" $
        hint emptyState (DMap [("coords", DList[DMap[("col", DInteger 5), ("harry", DInteger 4)]])]) @?= Left "Bad value in one on Hint's DMap values"
    , testCase "Wrong 2nd tuple's value" $
        hint emptyState (DMap [("coords", DList[DMap[("col", DInteger 5), ("row", DNull)]])]) @?= Left "Tuple most contain a string key and a DInteger value"
    , testCase "Too many tuples" $
        hint emptyState (DMap [("coords", DList[DMap[("col", DInteger 5), ("row", DInteger 1), ("head", DInteger 2)]])]) @?= Left "Too many values in one of Hint's DMaps"
    , testCase "Wrong type after first DMap" $
        hint emptyState (DMap [("coords", DList[DMap[("col", DInteger 5), ("row", DInteger 1)],DNull])]) @?= Left "DList must contain only DMaps with 2 tuples each"
    , testCase "Wrong type after first DMap" $
        hint emptyState (DMap [("coords", DList[DMap[("col", DInteger 5), ("row", DInteger 1)],DNull])]) @?= Left "DList must contain only DMaps with 2 tuples each"
    , testCase "Correct input" $
        hint emptyState (DMap [("coords", DList[DMap[("col", DInteger 5), ("row", DInteger 1)], DMap[("col", DInteger 6), ("row", DInteger 2)]])]) @?= Right (State 0 [] [] [Coord 5 1, Coord 6 2])
  ]