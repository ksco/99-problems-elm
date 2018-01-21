import Html exposing (text)
import List


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []        -> []
    (x :: xs) -> 
      if predicate x 
        then x :: takeWhile predicate xs
        else []

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
      [ (takeWhile isOdd [ 1, 2, 1 ] == [ 1 ])
      , (takeWhile isEven [ 1, 2, 1 ] == [])
      , (takeWhile isEven [] == [])
      , (takeWhile isEven [ 2, 4, 100000, 1 ] == [ 2, 4, 100000 ])
      , (takeWhile ((>) 5) (List.range 1 10) == [ 1, 2, 3, 4 ])
      , (takeWhile ((>) 50) (List.range 1 10) == List.range 1 10)
      ]

isEven : Int -> Bool
isEven x = x % 2 == 0

isOdd : Int -> Bool
isOdd x = x % 2 /= 0