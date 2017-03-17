module ExtDictTests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import ExtDict exposing (..)

tests : Test
tests =
  describe "Zipper Insertions"
    [ foldlIfTest
    ]

insert : comparable->ExtDict.Dict comparable String->ExtDict.Dict comparable String
insert key dict =
  ExtDict.insert key (toString key) dict

foldlIfCheck : String->List Int->Int->Int->List Int->Test
foldlIfCheck description list min max expected=
  let
    extDict = List.foldr insert ExtDict.empty list    
    results = ExtDict.foldlIf (rangeCompare min max) (\k v acc-> k::acc) [] extDict
  in
    test description (\_->Expect.equalLists (List.sort results) (List.sort expected))

rangeCompare : comparable->comparable->comparable->Order
rangeCompare min max x =
  case compare max x of
    LT-> LT
    _->case compare min x of
         GT->GT
         _->EQ
             
foldlIfTest : Test
foldlIfTest =
  describe "foldlIf tests"
  [
   test "EQ" (\_->Expect.equal (rangeCompare 4 6 5) EQ)
  , test "EQ" (\_->Expect.equal (rangeCompare 4 4 4) EQ)
  , test "GT" (\_->Expect.equal (rangeCompare 4 8 1) GT)
  , test "LT" (\_->Expect.equal (rangeCompare 4 8 9) LT)
         
  , foldlIfCheck "full key check" [1,2,3,4,5,6,7,8,9] 4 4 [4]
  , foldlIfCheck "key not found check" [1,2,3,4,5,6,7,8,9] 10 15 []
  , foldlIfCheck "sub key query" [1,2,3,4,5,6,7,8,9] 4 8 [4,5,6,7,8]
  , foldlIfCheck "sub key query min side" [1,2,3,4,5,6,7,8,9] 1 3 [1,2,3]
  , foldlIfCheck "sub key query max side" [1,2,3,4,5,6,7,8,9] 8 15 [8,9]
                 
  -- , foldlIfStringCheck "full key check" [1,2] [11,12] [101,102] [1,11,101] [[1,11,101]]
  -- , foldlIfStringCheck "key not found check" [1,2] [11,12] [101,102] [1,11,100] []
  -- , foldlIfStringCheck "sub key query 1 level" [1,2,3] [11,12,13] [101,102] [2,12] [[2,12,101],[2,12,102]]
  -- , foldlIfStringCheck "sub key query 2 level" [1,2] [11,12] [101,102] [2] [ [2,11,101]
  --                                                                          , [2,11,102]
  --                                                                          , [2,12,101]
  --                                                                          , [2,12,102]
  --                                                                          ]
  ]


-- makeCombinations : List a->List a->List a->List (List a)
-- makeCombinations listA listB listC =
--   lift3 (\a b c ->[a,b,c]) listA listB listC

-- foldlIfStringCheck : String->List Int->List Int->List Int->List Int->List (List Int)->Test
-- foldlIfStringCheck description list1 list2 list3 key expected=
--   let
--     combinations = makeCombinations list1 list2 list3
--     extDict = List.foldr insert ExtDict.empty combinations
--     results = ExtDict.foldlIf (sequenceCompare key) (\k v acc-> k::acc) [] extDict
--   in
--     test description (\_->Expect.equalLists (List.sort results) (List.sort expected))   
