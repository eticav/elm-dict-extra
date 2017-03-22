module ExtDict exposing
  ( Dict
  , empty, singleton, insert, update
  , isEmpty, get, remove, member, size
  , filter
  , partition
  , foldl, foldr, map
  , union, intersect, diff
  , keys, values
  , toList, fromList
  , foldlIf
  )

{-| A dictionary mapping unique keys to values. The keys can be any k
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of k types.

Insert, remove, and query operations all take *O(log n)* time.

# DictImplionaries
@docs DictImpl

# Build
@docs emptyImpl, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

# Combine
@docs union, intersect, diff

-}

import Basics exposing (..)
import Maybe exposing (..)
import List exposing (..)

import String



-- DICTIONARIES


-- BBlack and NBlack should only be used during the deletion
-- algorithm. Any other occurrence is a bug and should fail an assert.
type NColor
    = Red
    | Black
    | BBlack  -- Double Black, counts as 2 blacks for the invariant
    | NBlack  -- Negative Black, counts as -1 blacks for the invariant


type LeafColor
    = LBlack
    | LBBlack -- Double Black, counts as 2


{-| A dictionary of keys and values. So a `(DictImpl String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type Dict k v acc= Dict { dict : DictImpl k v
                        , compare : k->k->Order
                        , acc : acc
                        , initial : acc
                        }

type DictImpl k v
    = Node NColor k v (DictImpl k v) (DictImpl k v)
    | Empty LeafColor

{-| Create an emptyImpl dictionary. -}
empty : (k->k->Order)->acc->Dict k v acc
empty compareFun initial=
  Dict { dict = emptyImpl
       , compare =compareFun
       , acc =initial
       , initial=initial
       }
      
{-| Create an emptyImpl dictionary. -}
emptyImpl : DictImpl k v
emptyImpl =
  Empty LBlack


maxWithDefault : k -> v -> DictImpl k v -> (k, v)
maxWithDefault k v r =
  case r of
    Empty _ ->
      (k, v)

    Node _ kr vr _ rr ->
      maxWithDefault kr vr rr


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : k -> Dict k v acc-> Maybe v
get targetKey (Dict dict) =
  let
    getImpl : k -> DictImpl k v -> Maybe v
    getImpl targetKey dictImpl =      
      case dictImpl of
        Empty _ ->
          Nothing

        Node _ key value left right ->
          case dict.compare targetKey key of
            LT ->
              getImpl targetKey left
            EQ ->
              Just value                   
            GT ->
              getImpl targetKey right    
  in
    getImpl targetKey dict.dict

{-| Determine if a key is in a dictionary. -}
member : k -> Dict k v acc-> Bool
member key dict =
  case get key dict of
    Just _ ->
      True
    Nothing ->
      False


{-| Determine the number of key-value pairs in the dictionary. -}
size : DictImpl k v -> Int
size dict =
  sizeHelp 0 dict


sizeHelp : Int -> DictImpl k v -> Int
sizeHelp n dict =
  case dict of
    Empty _ ->
      n

    Node _ _ _ left right ->
      sizeHelp (sizeHelp (n+1) right) left


{-| Determine if a dictionary is emptyImpl.

    isEmpty emptyImpl == True
-}
isEmpty : DictImpl k v -> Bool
isEmpty dict =
  dict == emptyImpl


{- The actual pattern match here is somewhat lax. If it is given invalid input,
it will do the wrong thing. The expected behavior is:

  red node => black node
  black node => same
  bblack node => xxx
  nblack node => xxx

  black leaf => same
  bblack leaf => xxx
-}
ensureBlackRoot : DictImpl k v -> DictImpl k v
ensureBlackRoot dict =
  case dict of
    Node Red key value left right ->
      Node Black key value left right

    _ ->
      dict

{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision. -}
insert : k -> v -> Dict k v acc-> Dict k v acc
insert key value dict =
  update key (always (Just value)) dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made. -}
remove : k -> Dict k v acc -> Dict k v acc
remove key dict =
  update key (always Nothing) dict

         
type Flag = Insert | Remove | Same

          
{-| Update the value of a dictionary for a specific key with a given function. -}
update : k -> (Maybe v -> Maybe v) -> Dict k v acc-> Dict k v acc
update k alter (Dict dict) =
  let
    up dictImpl =
      case dictImpl of
        Empty _ ->
          case alter Nothing of
            Nothing ->
              (Same, emptyImpl)

            Just v ->
              (Insert, Node Red k v emptyImpl emptyImpl)

        Node clr key value left right ->
          case dict.compare k key of
            EQ ->
              case alter (Just value) of
                Nothing ->
                  (Remove, rem clr left right)
                Just newValue ->
                  (Same, Node clr key newValue left right)

            LT ->
              let (flag, newLeft) = up left in
              case flag of
                Same ->
                  (Same, Node clr key value newLeft right)
                Insert ->
                  (Insert, balance clr key value newLeft right)
                Remove ->
                  (Remove, bubble clr key value newLeft right)

            GT ->
              let (flag, newRight) = up right in
              case flag of
                Same ->
                  (Same, Node clr key value left newRight)
                Insert ->
                  (Insert, balance clr key value left newRight)
                Remove ->
                  (Remove, bubble clr key value left newRight)

    (flag, updatedDictImpl) =
      up dict.dict
  in
    case flag of
      Same ->
        Dict {dict|dict=updatedDictImpl}
      Insert ->
        Dict {dict|dict=ensureBlackRoot updatedDictImpl}
      Remove ->
        Dict {dict|dict=blacken updatedDictImpl}

             
singleton : (k->k->Order)->acc-> k -> v -> Dict k v acc
singleton compareFun initial key value =
  insert key value (empty compareFun initial)



-- HELPERS


isBBlack : DictImpl k v -> Bool
isBBlack dict =
  case dict of
    Node BBlack _ _ _ _ ->
      True

    Empty LBBlack ->
      True

    _ ->
      False


moreBlack : NColor -> NColor
moreBlack color =
  case color of
    Black ->
      BBlack

    Red ->
      Black

    NBlack ->
      Red

    BBlack ->
      Native.Debug.crash "Can't make a double black node more black!"


lessBlack : NColor -> NColor
lessBlack color =
  case color of
    BBlack ->
      Black

    Black ->
      Red

    Red ->
      NBlack

    NBlack ->
      Native.Debug.crash "Can't make a negative black node less black!"


{- The actual pattern match here is somewhat lax. If it is given invalid input,
it will do the wrong thing. The expected behavior is:

  node => less black node

  bblack leaf => black leaf
  black leaf => xxx
-}
lessBlackTree : DictImpl k v -> DictImpl k v
lessBlackTree dict =
  case dict of
    Node c k v l r ->
      Node (lessBlack c) k v l r

    Empty _ ->
      Empty LBlack


reportRemBug : String -> NColor -> String -> String -> a
reportRemBug msg c lgot rgot =
  Native.Debug.crash <|
    String.concat
    [ "Internal red-black tree invariant violated, expected "
    , msg, " and got ", toString c, "/", lgot, "/", rgot
    , "\nPlease report this bug to <https://github.com/elm-lang/core/issues>"
    ]


-- Remove the top node from the tree, may leave behind BBlacks
rem : NColor -> DictImpl k v -> DictImpl k v -> DictImpl k v
rem color left right =
  case (left, right) of
    (Empty _, Empty _) ->
      case color of
        Red ->
          Empty LBlack

        Black ->
          Empty LBBlack

        _ ->
          Native.Debug.crash "cannot have bblack or nblack nodes at this point"

    (Empty cl, Node cr k v l r) ->
      case (color, cl, cr) of
        (Black, LBlack, Red) ->
          Node Black k v l r

        _ ->
          reportRemBug "Black/LBlack/Red" color (toString cl) (toString cr)

    (Node cl k v l r, Empty cr) ->
      case (color, cl, cr) of
        (Black, Red, LBlack) ->
          Node Black k v l r

        _ ->
          reportRemBug "Black/Red/LBlack" color (toString cl) (toString cr)

    -- l and r are both Nodes
    (Node cl kl vl ll rl, Node _ _ _ _ _) ->
      let
        (k, v) =
          maxWithDefault kl vl rl

        newLeft =
          removeMax cl kl vl ll rl
      in
        bubble color k v newLeft right


-- Kills a BBlack or moves it upward, may leave behind NBlack
bubble : NColor -> k -> v -> DictImpl k v -> DictImpl k v -> DictImpl k v
bubble c k v l r =
  if isBBlack l || isBBlack r then
    balance (moreBlack c) k v (lessBlackTree l) (lessBlackTree r)

  else
    Node c k v l r


-- Removes rightmost node, may leave root as BBlack
removeMax : NColor -> k -> v -> DictImpl k v -> DictImpl k v -> DictImpl k v
removeMax c k v l r =
  case r of
    Empty _ ->
      rem c l r

    Node cr kr vr lr rr ->
      bubble c k v l (removeMax cr kr vr lr rr)


-- generalized tree balancing act
balance : NColor -> k -> v -> DictImpl k v -> DictImpl k v -> DictImpl k v
balance c k v l r =
  let
    tree =
      Node c k v l r
  in
    if blackish tree then
      balanceHelp tree

    else
      tree


blackish : DictImpl k v -> Bool
blackish t =
  case t of
    Node c _ _ _ _ ->
      c == Black || c == BBlack

    Empty _ ->
      True


balanceHelp : DictImpl k v -> DictImpl k v
balanceHelp tree =
  case tree of
    -- double red: left, left
    Node col zk zv (Node Red yk yv (Node Red xk xv a b) c) d ->
      balancedTree col xk xv yk yv zk zv a b c d

    -- double red: left, right
    Node col zk zv (Node Red xk xv a (Node Red yk yv b c)) d ->
      balancedTree col xk xv yk yv zk zv a b c d

    -- double red: right, left
    Node col xk xv a (Node Red zk zv (Node Red yk yv b c) d) ->
      balancedTree col xk xv yk yv zk zv a b c d

    -- double red: right, right
    Node col xk xv a (Node Red yk yv b (Node Red zk zv c d)) ->
      balancedTree col xk xv yk yv zk zv a b c d

    -- handle double blacks
    Node BBlack xk xv a (Node NBlack zk zv (Node Black yk yv b c) (Node Black _ _ _ _ as d)) ->
      Node Black yk yv (Node Black xk xv a b) (balance Black zk zv c (redden d))

    Node BBlack zk zv (Node NBlack xk xv (Node Black _ _ _ _ as a) (Node Black yk yv b c)) d ->
      Node Black yk yv (balance Black xk xv (redden a) b) (Node Black zk zv c d)

    _ ->
      tree


balancedTree : NColor -> k -> v -> k -> v -> k -> v -> DictImpl k v -> DictImpl k v -> DictImpl k v -> DictImpl k v -> DictImpl k v
balancedTree col xk xv yk yv zk zv a b c d =
  Node
    (lessBlack col)
    yk
    yv
    (Node Black xk xv a b)
    (Node Black zk zv c d)


-- make the top node black
blacken : DictImpl k v -> DictImpl k v
blacken t =
  case t of
    Empty _ ->
      Empty LBlack

    Node _ k v l r ->
      Node Black k v l r


-- make the top node red
redden : DictImpl k v -> DictImpl k v
redden t =
  case t of
    Empty _ ->
      Native.Debug.crash "can't make a Leaf red"

    Node _ k v l r ->
      Node Red k v l r



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict k v acc -> Dict k v acc -> Dict k v acc
union t1 t2 =
  foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Dict k v acc -> Dict k v acc -> Dict k v acc
intersect t1 t2 =
  filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict k v acc-> Dict k v acc -> Dict k v acc
diff t1 t2 =
  foldl (\k v t -> remove k t) t1 t2


-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> DictImpl k a -> DictImpl k b
map f dict =
  case dict of
    Empty _ ->
      Empty LBlack

    Node clr key value left right ->
      Node clr key (f key value) (map f left) (map f right)


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (k -> v -> b -> b) -> b -> Dict k v acc-> b
foldl f acc (Dict dict) =
  foldlImpl f acc dict.dict
    
foldlImpl : (k -> v -> b -> b) -> b -> DictImpl k v -> b
foldlImpl f acc dict =
  case dict of
    Empty _ ->
      acc

    Node _ key value left right ->
      foldlImpl f (f key value (foldlImpl f acc left)) right


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (k -> v -> b -> b) -> b -> Dict k v acc -> b
foldr f acc (Dict dict) =
  foldrImpl f acc dict.dict

foldrImpl : (k -> v -> b -> b) -> b -> DictImpl k v -> b
foldrImpl f acc t =
  case t of
    Empty _ ->
      acc

    Node _ key value left right ->
      foldrImpl f (f key value (foldrImpl f acc right)) left


{-| Keep a key-value pair when it satisfies a predicate. -}
filter : (k -> v -> Bool) -> Dict k v acc-> Dict k v acc
filter predicate (Dict dict) =
  let
    add key value dict =
      if predicate key value then
        insert key value dict
      else
        dict
  in
    foldl add (empty dict.compare dict.initial) (Dict dict)


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (k -> v -> Bool) -> Dict k v acc -> (Dict k v acc, Dict k v acc)
partition predicate (Dict dict) =
  let
    add key value (t1, t2) =
      if predicate key value then
        (insert key value t1, t2)

      else
        (t1, insert key value t2)
  in
    foldl add (empty dict.compare dict.initial, empty dict.compare dict.initial) (Dict dict)

-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]
-}
keys : Dict k v acc-> List k
keys dict =
  foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]
-}
values : Dict k v acc-> List v
values dict =
  foldr (\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys. -}
toList : Dict k v acc-> List (k,v)
toList dict =
  foldr (\key value list -> (key,value) :: list) [] dict


{-| Convert an association list into a dictionary. -}
fromList : List (k,v)->Dict k v acc-> Dict k v acc
fromList assocs dict =
  List.foldl (\(key,value) xdict -> insert key value xdict) dict assocs

{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key. Traversal will only occur on keys correponding to the filter function.

    example :
    
    rangeCompare : k->k->k->Order
    rangeCompare min max x =
      case compare max x of
        LT-> LT
        _->case compare min x of
             GT->GT
             _->EQ
    
    foldlIf (rangeCompare 5 8) (\k v acc-> k::acc) [] fromList [4,5,6,7,8,9]  -- [5,6,7,8]    
-}

foldlIf : (k->Order)->(k -> v -> b -> b) -> b -> Dict k v acc-> b
foldlIf comp f acc (Dict dict) =
  foldlIfImpl comp f acc dict.dict
          
foldlIfImpl : (k->Order)->(k -> v -> b -> b) -> b -> DictImpl k v-> b
foldlIfImpl comp f acc dict =
  case dict of
    Empty _ ->
      acc
    Node _ key value left right ->
      case comp key of
        LT -> foldlIfImpl comp f acc left
        EQ ->
          foldlIfImpl comp f (f key value (foldlIfImpl comp f acc left)) right
        GT -> foldlIfImpl comp f acc right
