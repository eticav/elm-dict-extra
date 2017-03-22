module ExtDictTests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import ExtDict exposing (..)

tests : Test
tests =
  describe "Zipper Insertions"
    [ foldlIfTest
    , accumulateTest
    ]
type alias Acc = { size: Int
                 , inserts : Int
                 , updates : Int
                 , removes : Int
                 }
    
initialAcc = { size=0
             , inserts=0
             , updates=0
             , removes=0
             }
  
insert : comparable->ExtDict.Dict comparable String Acc->ExtDict.Dict comparable String Acc
insert key dict =
  ExtDict.insert key (toString key) dict

remove : comparable->ExtDict.Dict comparable String Acc->ExtDict.Dict comparable String Acc
remove key dict =
  ExtDict.remove key dict

update : comparable->ExtDict.Dict comparable String Acc->ExtDict.Dict comparable String Acc
update key dict =
  ExtDict.update key (\str ->
                        case str of
                          Just s -> Just (toString key)
                          Nothing -> Nothing
                     ) dict
  

foldlIfCheck : String->List Int->Int->Int->List Int->Test
foldlIfCheck description list min max expected=
  let
    extDict = List.foldr insert (ExtDict.empty Basics.compare initialAcc) list    
    results = ExtDict.foldlIf (rangeCompare min max) (\k v acc-> k::acc) [] extDict
  in
    test description (\_->Expect.equalLists (List.sort results) (List.sort expected))

accumulateTest : Test
accumulateTest =
  describe "accumulat tests"
    [
     accumulateCheck "5 inserts 5 removes" [1,2,3,4,5] [1,2,3,4,5] [] 5 5 0
    , accumulateCheck "5 inserts 3 removes 2 updates" [1,2,3,4,5] [1,2,3] [4,5] 5 3 2
    , accumulateCheck "2 updates on empty list" [] [] [4,5] 0 0 0
    , accumulateCheck "2 fail updates, 2 success updates " [1,2,3,4,5,6,7,8] [7,8] [4,5,7,8] 8 2 2
    , accumulateCheck "2 fail removes on empty list" [] [7,8] [] 0 0 0
    , accumulateCheck "2 fail removes" [1,2,3] [7,8] [] 3 0 0
    , accumulateCheck "2 fail removes" [1,2,2] [7,8] [] 2 0 1
    ]

accumulateCheck : String->List Int->List Int->List Int->Int->Int->Int->Test
accumulateCheck description listInserts listRemoves listUpdates inserts removes updates=
  let
    dict = ExtDict.empty Basics.compare initialAcc
         |> ExtDict.accumulate
            (\(k,v) acc->{acc|inserts=acc.inserts+1,size=acc.size+1})
            (\(k,v) acc->{acc|removes=acc.removes+1,size=acc.size-1})
            (Just (\_ _ acc->{acc|updates=acc.updates+1}))
                                    
    insDict = List.foldr insert dict listInserts
    remDict = List.foldr remove  insDict listRemoves
    upDict = List.foldr update  remDict listUpdates
              
    results = ExtDict.accumulation upDict
  in
    describe description
    [ test "inserts" (\_->Expect.equal results.inserts inserts)
    , test "removes" (\_->Expect.equal results.removes removes)
    , test "updates" (\_->Expect.equal results.updates updates)
    , test "size" (\_->Expect.equal results.size (results.inserts-results.removes))
    ]                 
    
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
