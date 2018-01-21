import Html exposing (Html, text)
import List exposing (..)

elemLengthNums : List (List a) -> List a -> Int
elemLengthNums list elem = 
  list
    |> filter (\a -> length a == length elem)
    |> length

sortByLengthFrequency : List (List a) -> List (List a)
sortByLengthFrequency xs = sortBy (elemLengthNums xs) xs

{--
main : Html msg
main = text <| toString <| sortByLengthFrequency [ [ 1 ], [ 2 ], [ 3 ], [ 6, 7, 8 ], [ 2, 34, 5 ], [] ]
--}

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
      [ (List.map List.length
        <| sortByLengthFrequency [ [ 1 ], [ 2 ], [ 3 ], [ 6, 7, 8 ], [ 2, 34, 5 ], [] ]
        )
        == [ 0, 3, 3, 1, 1, 1 ]
      , (List.map List.length
        <| sortByLengthFrequency [ [ 1 ], [ 2 ], [ 3 ], [ 6 ], [ 2 ], (List.range 1 10) ]
        )
        == [ 10, 1, 1, 1, 1, 1 ]
      , (List.map List.length
        <| sortByLengthFrequency [ [ 1, 2, 3 ], [ 6, 7, 8 ], [ 0 ], [ 2, 3, 5 ] ]
        )
        == [ 1, 3, 3, 3 ]
      , (List.map List.length
        <| sortByLengthFrequency [ [] ]
        )
        == [ 0 ]
      ]


(..) : Int -> Int -> List Int
(..) start end =
  List.range start end