import Html
import List exposing (..)

runLengths : List (List a) -> List ( Int, a )
runLengths xss =
  foldr 
    (\a b -> 
      case a of
        []        -> b
        (x :: xs) -> (length xs + 1, x) :: b 
    )
    []
    xss

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
      [ runLengths [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ]
        == [ ( 4, 1 ), ( 1, 2 ), ( 2, 5 ), ( 1, 2 ), ( 1, 1 ) ]
      , runLengths [ [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ]
        == [ ( 1, 2 ), ( 2, 5 ), ( 1, 2 ), ( 1, 1 ) ]
      , runLengths [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ] ]
        == [ ( 4, 1 ), ( 1, 2 ), ( 2, 5 ) ]
      , runLengths [ [ 1, 1, 1, 1 ] ]
        == [ ( 4, 1 ) ]
      , runLengths [ [ "a", "a", "a", "a" ], [ "b" ], [ "c", "c" ], [ "b" ], [ "a" ] ]
        == [ ( 4, "a" ), ( 1, "b" ), ( 2, "c" ), ( 1, "b" ), ( 1, "a" ) ]
      , runLengths [ [] ] == []
      , runLengths [ [], [ "a", "a" ] ] == [ ( 2, "a") ]
      , runLengths [] == []
      ]