import Html exposing (..)
import List exposing (..)

repeatElements : Int -> List a -> List a
repeatElements count list = foldr (\a b -> repeat count a ++ b) [] list

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
      [ repeatElements 2 [ 1, 2, 5, 5, 2, 1 ] == [ 1, 1, 2, 2, 5, 5, 5, 5, 2, 2, 1, 1 ]
      , repeatElements 4 [ 1, 2 ] == [ 1, 1, 1, 1, 2, 2, 2, 2 ]
      , repeatElements 4 [] == []
      , repeatElements 0 [ 1, 2 ] == []
      , repeatElements (-1) [ 1, 2 ] == []
      , repeatElements 40 [ 1 ] == [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
      , repeatElements 4 [ "1", "2" ] == [ "1", "1", "1", "1", "2", "2", "2", "2" ]
      ]