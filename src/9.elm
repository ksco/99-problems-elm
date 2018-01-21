import Html
import List

takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []        -> []
    (x :: xs) -> 
      if predicate x 
        then x :: takeWhile predicate xs
        else []

dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
  case list of
    []        -> []
    (x :: xs) -> 
      if predicate x 
        then dropWhile predicate xs
        else x::xs

pack : List a -> List (List a)
pack list =
  case list of
    []        -> []
    (x :: xs) -> 
      [x :: takeWhile ((==) x) xs] ++ 
        (pack <| dropWhile ((==) x) xs)

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
      [ pack [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ] == [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ]
      , pack [ 2, 1, 1, 1 ] == [ [ 2 ], [ 1, 1, 1 ] ]
      , pack [ 2, 2, 2, 1, 1, 1 ] == [ [ 2, 2, 2 ], [ 1, 1, 1 ] ]
      , pack [ 1 ] == [ [ 1 ] ]
      , pack [] == []
      , pack [ "aa", "aa", "aa" ] == [ [ "aa", "aa", "aa" ] ]
      , pack [ "aab", "b", "b", "aa" ] == [ [ "aab" ], [ "b", "b" ], [ "aa" ] ]
      ]