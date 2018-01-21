import Html exposing (text)
import List exposing (..)


myReverse : List a -> List a
myReverse xs = foldl (\a l -> a :: l) [] xs

main : Html.Html a    
main =
  Html.text 
    <| case test of 
      0 -> 
        "Your implementation passed all tests."

      1 -> "Your implementation failed one test."

      n -> "Your implementation failed " ++ toString n ++ " tests." 


test : Int
test =
  List.length
    <| List.filter ((==) False)
      [ myReverse [1, 2, 3, 4] == [4, 3, 2, 1] 
      , myReverse [2, 1] == [1, 2] 
      , myReverse [1] == [1] 
      , myReverse [] == [] 
      , myReverse [ 'a', 'b', 'c' ] == [ 'c', 'b', 'a' ]
      ]