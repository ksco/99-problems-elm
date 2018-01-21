import Html
import List exposing (..)

noDupes : List a -> List a
noDupes list =
  foldr 
    (\a b -> 
      case b of
        (x :: _) -> if a == x then b else a :: b
        []       -> [a]
    )
    []
    list

main : Html.Html a
main =
  Html.text
    <| case test of
      0 ->
        "Your implementation passed all tests."

      1 ->
        "Your implementation failed one test."

      x ->
        "Your implementation failed " ++ (toString x) ++ " tests."


test : Int
test =
  List.length
    <| List.filter ((==) False)
      [ noDupes [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ] == [ 1, 2, 5, 2, 1 ]
      , noDupes [ 2, 1, 1, 1 ] == [ 2, 1 ]
      , noDupes [ 2, 2, 2, 1, 1, 1 ] == [ 2, 1 ]
      , noDupes [ 1 ] == [ 1 ]
      , noDupes [] == []
      , noDupes [ "aa", "aa", "aa" ] == [ "aa" ]
      , noDupes [ "aab", "b", "b", "aa" ] == [ "aab", "b", "aa" ]
      ]