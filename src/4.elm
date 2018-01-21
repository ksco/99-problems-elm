import Html
import List exposing (..)

countElements : List a -> Int
countElements xs = foldl (\_ b -> b + 1) 0 xs 

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
      [ countElements (List.range 1 4000) == 4000
      , countElements [ 1 ] == 1
      , countElements [] == 0
      , countElements [ 'a', 'b', 'c' ] == 3
      ]