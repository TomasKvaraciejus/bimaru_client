import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib2 (renderDocument, gameStart, hint)
import Lib1 (State(..))
import Types (Document(..), Coord(..))


main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  gameStartTests,
  hintTests])

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [
    testCase "DInteger" $
        renderDocument (DInteger 5) @?= "5"   
    , testCase "DNull" $
        renderDocument DNull @?= "~"
    , testCase "DString" $
        renderDocument (DString "hello") @?= "hello"
    , testCase "DMap" $
        renderDocument (DMap[("Hello", DInteger 3), ("Bye", DInteger 2)]) @?= "[{Hello: 3}, {Bye: 2}]"
    , testCase "DMap nested DMap" $
        renderDocument (DMap[("Hello", DMap[("Bye", DString "red")])]) @?= "[{Hello: [{Bye: red}]}]"
    , testCase "DMap nested DList" $
        renderDocument (DMap[("Hello", DList[DInteger 4, DString "good", DMap[("Wow", DNull)]])]) @?= "[{Hello: [4, good, [{Wow: ~}]]}]"
    , testCase "DList" $
        renderDocument (DList[DInteger 1, DNull, DString "hello"]) @?= "[1, ~, hello]"
    , testCase "DList nested DList" $
        renderDocument (DList[DNull, DString "red", DList[DInteger 4, DInteger 0, DMap[("Hello", DString "goodbye")]]]) @?= "[~, red, [4, 0, [{Hello: goodbye}]]]"
    , testCase "Coord List" $
        renderDocument (DMap[("coords", DList [DMap[("col: ", DInteger 1), ("row: ", DInteger 1)]])]) @?= "{coords: [{col: 1, row: 1}]}"
    
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

wrongState :: String
wrongState = "Wrong State starting data"

ending :: String
ending = " key not found or is placed incorrectly"

emptyState :: State
emptyState = State 0 [] [] []

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