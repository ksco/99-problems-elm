import Html
import List exposing (..)

myReverse : List a -> List a
myReverse xs = foldl (\a l -> a :: l) [] xs

isPalindrome : List a -> Bool
isPalindrome xs = myReverse xs == xs

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
      [ isPalindrome [ 1, 3, 5, 8, 5, 3, 1 ] == True
      , isPalindrome [ 2, 1 ] == False
      , isPalindrome [ 1 ] == True
      , isPalindrome [] == True
      , isPalindrome [ "aa", "bb", "aa" ] == True
      , isPalindrome [ "aab", "b", "aa" ] == False
      ]