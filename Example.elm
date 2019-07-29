module Example exposing (..)
import FoldPerso exposing (..)
import Fuzz exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)




testsFold : Test
testsFold =   describe "Fold"
        [ describe "T1"
            [fuzz (list (intRange -15 15)) "reverse" <|
                    \l ->
                        l
                            |> reverseG
                            |> Expect.equal (List.reverse l)
            ,fuzz (list (intRange -15 15)) "all" <|
                    \l ->
                        l
                            |> allFold (\x -> modBy 2 x == 0 ) 
                            |> Expect.equal (List.all (\x -> modBy 2 x == 0 ) l)
            
            ,fuzz(list (intRange -15 15))"any" <|
                \l->
                    l
                        |>anyFold (\x -> modBy 2 x == 0 )
                        |> Expect.equal (List.any (\x -> modBy 2 x == 0 )  l)
            ]
        ]


testsUnfold : Test
testsUnfold =   describe "Unfold"
        [ describe "Tests Unfold"
            [fuzz (list int) "filter unfold" <|
                    \l ->
                        l
                            |> filterUnfold (\x -> modBy 2 x /= 0 )  
                            |> Expect.equal (List.filter (\x -> modBy 2 x /= 0 ) l)
            ,fuzz (list string) "filter map unfold" <|
                    \l ->
                        l
                            |> filterMapUnfold (\x -> String.toInt x )  
                            |> Expect.equal (List.filterMap (\x -> String.toInt x ) l)
            
            ,fuzz(list int ) "map unfold" <|
                \l->
                    l
                        |>mapUnfold (\x -> 2*x)  
                        |> Expect.equal (List.map (\x -> 2*x)   l)
            ]
        ]

