import Html exposing (text)
import List exposing (map)

and_ : Bool -> Bool -> Bool
and_ = (&&)

or_ : Bool -> Bool -> Bool
or_ = (||)

nand_ : Bool -> Bool -> Bool
nand_ a b = not <| a && b

nor_ : Bool -> Bool -> Bool
nor_ a b = not <| a || b

xor_ : Bool -> Bool -> Bool
xor_ a b = not <| a == b

implies : Bool -> Bool -> Bool
implies a b = if a then b else True

equivalent : Bool -> Bool -> Bool
equivalent = (==)


truthTable : (Bool -> Bool -> Bool) -> List ( Bool, Bool, Bool )
truthTable f =
  [ ( True, True, f True True )
  , ( True, False, f True False )
  , ( False, True, f False True )
  , ( False, False, f False False )
  ]

main : Html.Html a
main =
  text
    (if (test) then
      "Your implementation passed all tests."
     else
      "Your implementation failed at least one test."
    )


test : Bool
test =
  List.all ((==) True)
    [ truthTable and_
      == [ ( True, True, True )
         , ( True, False, False )
         , ( False, True, False )
         , ( False, False, False )
         ]
    , truthTable or_
      == [ ( True, True, True )
         , ( True, False, True )
         , ( False, True, True )
         , ( False, False, False )
         ]
    , truthTable nand_
      == [ ( True, True, False )
         , ( True, False, True )
         , ( False, True, True )
         , ( False, False, True )
         ]
    , truthTable nor_
      == [ ( True, True, False )
         , ( True, False, False )
         , ( False, True, False )
         , ( False, False, True )
         ]
    , truthTable xor_
      == [ ( True, True, False )
         , ( True, False, True )
         , ( False, True, True )
         , ( False, False, False )
         ]
    , truthTable implies
      == [ ( True, True, True )
         , ( True, False, False )
         , ( False, True, True )
         , ( False, False, True )
         ]
    , truthTable equivalent
      == [ ( True, True, True )
         , ( True, False, False )
         , ( False, True, False )
         , ( False, False, True )
         ]
    , truthTable (\a b -> (and_ a (or_ a b)))
      == [ ( True, True, True )
         , ( True, False, True )
         , ( False, True, False )
         , ( False, False, False )
         ]
    ]