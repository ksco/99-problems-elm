import Html
import List

range : Int -> Int -> List Int 
range start end = 
  if start > end 
    then start :: range (start-1) end
    else if start == end
    then [start]
    else start :: range (start+1) end

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
      [ range 1 5 == [1, 2, 3, 4, 5]
      , range 0 5 == [0, 1, 2, 3, 4, 5]
      , range -1 5 == [-1, 0, 1, 2, 3, 4, 5]
      , range 5 -1 == [ 5, 4, 3, 2, 1, 0, -1 ]
      , range 5 5 == [ 5 ]
      , List.length (range 1 999) == 999
      ]