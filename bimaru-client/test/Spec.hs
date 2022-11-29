import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encode )

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
      \doc -> parseDocument (cs (Y.encode doc)) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [  
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [   testCase "null" $
          parseDocument "~" @?= Right DNull
    , testCase "DList" $
        parseDocument "John:\n  - Hello:\n    - 1\n    - 2\n    - kello: null\n      priviet: 5\n    - t\n    - q\n  - Hu: null\n    John: 1\n    hi: hello\ngame: o\n" @?=
          Right (DMap[("John", DList[DMap[("Hello", DList[DInteger 1, DInteger 2, DMap[("kello", DNull), ("priviet", DInteger 4)], DString "t", DString "q"])], DMap[("Hu", DNull), ("John", DInteger 1), ("hi", DString "hello")]]), ("game", DString "o")])
    , testCase "DMap - 1 element" $
        parseDocument "John: 5\n" @?= Right (DMap[("John", DInteger 5)])
    , testCase "DMap - 2 elements" $
        parseDocument "John: 5\nStacey: Konichiwa\n" @?= Right (DMap[("John", DInteger 5), ("Stacey", DString "Konichiwa")])
    , testCase "DMap - 3 elements" $
        parseDocument "John: 5\nStacey: Konichiwa\nJessica: ~\n" @?= Right (DMap[("John", DInteger 5), ("Stacey", DString "Konichiwa"), ("Jessica", DNull)])
    , testCase "DMap - 2 elements, nested" $
        parseDocument "John:\n  Stacey: 5\n" @?= Right (DMap[("John", DMap[("Stacey", DInteger 5)])])
    , testCase "DMap - 3 elements, nested inside" $
        parseDocument "John:\n  Stacey: 5\n  Jessica: ~\n" @?= Right (DMap[("John", DMap[("Stacey", DInteger 5), ("Jessica", DNull)])])
    , testCase "DMap - 3 elements, nested outside" $
        parseDocument "John:\n  Stacey: 5\nJessica: ~\n" @?= Right (DMap[("John", DMap[("Stacey", DInteger 5)]), ("Jessica", DNull)])
    , testCase "Dmap - many nested" $
        parseDocument "John:\n  Stacey:\n    Jessica:\n      Kelly: 5" @?= Right (DMap[("John", DMap[("Stacey", DMap[("Jessica", DMap[("Kelly", DInteger 5)])])])])
    , testCase "Dlist - 1 element" $ 
        parseDocument "- John\n" @?= Right (DList[DString "John"])
    , testCase "Dlist - 2 elements" $ 
        parseDocument "- John\n- 2\n" @?= Right (DList[DString "John", DInteger 2])
    , testCase "Dlist - 3 elements" $ 
        parseDocument "- John\n- Kelly\n- Jeremy\n" @?= Right (DList[DString "John", DString "Kelly", DString "Jeremy"])
    , testCase "DList - 2 lists nested" $
        parseDocument "- John:\n  - Jeremy\n" @?= Right (DList[DString "John", DList[DString "Jeremy"]])
    , testCase "DList & DMap - 2 elements nested" $
        parseDocument "John:\n  - Jeremy: 5\n" @?= Right (DMap[("John", DList[DMap[("Jeremy", DInteger 5)]])])
    , testCase "DMap & DList - 2 elements, nested" $
        parseDocument "John:\n  - 5\n" @?= Right (DMap[("John", DList[DInteger 5])])
    , testCase "DMap & DList - 3 elements, nested" $
        parseDocument "John:\n  - 5\n  - Stacey\n" @?= Right (DMap[("John", DList[DInteger 5, DString "Stacey"])])
        
        -- negative integers
        -- " -x" - error (spacing after dash)

    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "~"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    , testCase "Big boi" $
        renderDocument (DMap[("game_setup_id", DString "0de28b51-e8ef-41d5-a1e6-131b51c4a638"), ("number_of_hints", DInteger 10), ("occupied_rows", DMap[("head", DInteger 3), ("tail", DMap[("head", DInteger 3), ("tail", DMap[("head", DInteger 0), ("tail", DMap[("head", DInteger 0), ("tail", DMap[("head", DInteger 3), ("tail", DMap[("head", DInteger 0), ("tail", DMap[("head", DInteger 5), ("tail", DMap[("head", DInteger 0), ("tail", DMap[("head", DInteger 4), ("tail", DMap[("head", DInteger 2), ("tail", DNull)])])])])])])])])])]), ("occupied_cols", DMap[("head", DInteger 1), ("tail", DMap[("head", DInteger 1), ("tail", DMap[("head", DInteger 4), ("tail", DMap[("head", DInteger 2), ("tail", DMap[("head", DInteger 2), ("tail", DMap[("head", DInteger 2), ("tail", DMap[("head", DInteger 2), ("tail", DMap[("head", DInteger 2), ("tail", DMap[("head", DInteger 0), ("tail", DMap[("head", DInteger 4), ("tail", DNull)])])])])])])])])])])]) @?=
          "game_setup_id: 0de28b51-e8ef-41d5-a1e6-131b51c4a638\nnumber_of_hints: 10\noccupied_rows:\n  head: 3\n  tail:\n    head: 3\n    tail:\n      head: 0\n      tail:\n        head: 0\n        tail:\n          head: 3\n          tail:\n            head: 0\n            tail:\n              head: 5\n              tail:\n                head: 0\n                tail:\n                  head: 4\n                  tail:\n                    head: 2\n                    tail: ~\noccupied_cols:\n  head: 1\n  tail:\n    head: 1\n    tail:\n      head: 4\n      tail:\n        head: 2\n        tail:\n          head: 2\n          tail:\n            head: 2\n            tail:\n              head: 2\n              tail:\n                head: 2\n                tail:\n                  head: 0\n                  tail:\n                    head: 4\n                    tail: ~\n"
    , testCase "Dlist" $
        renderDocument (DMap[("John", DList[DString "Hello", DList[DInteger 1, DInteger 2, DMap[("kello", DNull), ("priviet", DInteger 5)], DString "t", DString "q"], DMap[("Hu", DNull), ("John", DInteger 1), ("hi", DString "hello")]]), ("game", DString "o")]) @?=
          "John:\n  - Hello:\n    - 1\n    - 2\n    - kello: ~\n      priviet: 5\n    - t\n    - q\n  - Hu: ~\n    John: 1\n    hi: hello\ngame: o\n"
    , testCase "DList in DList" $
        renderDocument (DMap[("John", DMap[("Hello", DList[ DString "Nice", DList[DInteger 1, DInteger 2, DMap[("kello", DNull), ("priviet", DInteger 5), ("not", DString "t"), ("map", DMap[("Hu", DMap[("John", DInteger 1), ("yes", DNull)])])], DInteger 5]]), ("game", DString "o")])]) @?=
          "John:\n  Hello:\n    - Nice:\n      - 1\n      - 2\n      - kello: ~\n        priviet: 5\n        not: t\n        map:\n          Hu:\n            John: 1\n            yes: ~\n      - 5\n  game: o\n"
    , testCase "DList in DMap" $
        renderDocument (DList[DInteger 4, DInteger 9, DMap[("u", DMap[("red", DList[DMap[("Hello", DInteger 9)], DMap[("green", DInteger 6), ("brown", DList[DMap[("not", DInteger 5)], DMap[("notnice", DInteger 6)]]), ("nice", DString "nice")]])])], DNull]) @?=
          "- 4\n- 9\n- u:\n    red:\n      - Hello: 9\n      - green: 6\n        brown:\n          - not: 5\n          - notnice: 6\n        nice: nice\n- ~\n"
    , testCase "DInteger" $
        renderDocument (DInteger 5) @?= "5"   
    , testCase "DNull" $
        renderDocument DNull @?= "~"
    , testCase "DString" $
        renderDocument (DString "hello") @?= "hello"
    , testCase "DMap" $
        renderDocument (DMap[("Hello", DInteger 3)]) @?= "Hello: 3\n"
    , testCase "DMap 2" $
        renderDocument (DMap[("Hello", DInteger 3), ("Hello", DInteger 4)]) @?= "Hello: 3\nHello: 4\n"
    , testCase "DMap nested DMap" $
        renderDocument (DMap[("Hello", DMap[("Bye", DString "red bird")])]) @?= "Hello:\n  Bye: red bird\n"
    , testCase "DMap nested DList" $
        renderDocument (DMap[("Hello", DList[DInteger 4, DString "good", DMap[("Wow", DNull)]])]) @?= "Hello:\n  - 4\n  - good\n  - Wow: ~\n"
    , testCase "DList" $
        renderDocument (DList[DInteger 1, DNull, DString "hello"]) @?= "- 1\n- ~\n- hello\n"
    , testCase "DList nested DList" $
        renderDocument (DList[DNull, DString "red", DList[DInteger 4, DInteger 0, DMap[("Hello", DString "goodbye")]]]) @?= "- ~\n- red:\n  - 4\n  - 0\n  - Hello: goodbye\n"
    , testCase "DList nested DList 2" $
        renderDocument (DList[DList[DList[DList[DString "a"]]]]) @?= "        - a\n"
    , testCase "DList nested DList 3" $
        renderDocument (DList[DString "aa", DList[DList[DString "aaa", DList[DString "a"]]]]) @?= "- aa:\n    - aaa:\n      - a\n"
    , testCase "Coord List" $
        renderDocument (DMap[("coords", DList [DMap[("col: ", DInteger 1), ("row: ", DInteger 1)]])]) @?= "coords:\n- col: 1\n  row: 1\n"
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