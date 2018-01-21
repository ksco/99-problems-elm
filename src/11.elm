import Html exposing (..)
import List exposing (..)

type RleCode a
  = Run Int a
  | Single a

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

rleEncode : List a -> List (RleCode a)
rleEncode list =
  foldr
    (\a b ->
      case a of
        [] -> b
        (x :: xs) -> 
          (case length xs of
            0 -> Single x
            n -> Run (n+1) x) :: b
    )
    []
    (pack list)

{--
main : Html a
main = text <| toString <| rleEncode [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ]
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
      [ rleEncode [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ]
        == [ Run 4 1, Single 2, Run 2 5, Single 2, Single 1 ]
      , rleEncode [ 2, 1, 1, 1 ] == [ Single 2, Run 3 1 ]
      , rleEncode [ 2, 2, 2, 1, 1, 1 ] == [ Run 3 2, Run 3 1 ]
      , rleEncode [ 1 ] == [ Single 1 ]
      , rleEncode [] == []
      , rleEncode [ "aa", "aa", "aa" ] == [ Run 3 "aa" ]
      , rleEncode [ "aab", "b", "b", "aa" ]
        == [ Single "aab", Run 2 "b", Single "aa" ]
      ]