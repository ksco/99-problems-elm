import Html exposing (..)
import List exposing (..)

combinations : Int -> List a -> List (List a)
combinations n list =
  if n <= 0
    then [[]]
    else
      case list of
        []        -> []
        (x :: xs) -> List.map ((::) x) (combinations (n-1) xs) ++ combinations n xs

main : Html a
main =
  text
    <| case test of
      0 ->
        "Your implementation passed all tests."

      1 ->
        "Your implementation failed one test."

      x ->
        "Your implementation failed " ++ (toString x) ++ " tests."

test : Int
test =
  length
    <| filter ((==) False)
      [ combinations 1 (range 1 5) == [ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ] ]
      , combinations 2 [ 'a', 'b', 'c' ] == [ [ 'a', 'b' ], [ 'a', 'c' ], [ 'b', 'c' ] ]
      , combinations 2 (range 1 3) == [ [ 1, 2 ], [ 1, 3 ], [ 2, 3 ] ]
      , combinations 2 (range 1 4) == [ [ 1, 2 ], [ 1, 3 ], [ 1, 4 ], [ 2, 3 ], [ 2, 4 ], [ 3, 4 ] ]
      , combinations 0 (range 1 10) == [ [] ]
      , combinations -1 (range 1 10) == [ [] ]
      , length (combinations 3 (range 1 12)) == 220
      , length (combinations 4 (range 1 15)) == 1365
      ]