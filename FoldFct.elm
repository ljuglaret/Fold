module FoldFct exposing(..)

sommeList : List Int -> Int 
sommeList l = 
     let 
        aux  acc l0 = 
            case l0 of 
                [] -> acc 
                x::s -> aux (acc+x) s
    in aux 0 l 

inverseList : List a -> List a 
inverseList l = 
    let 
        aux acc l0 = 
            case l0 of 
                []  -> acc 
                x::s -> aux  (x::acc) s
    in aux [] l





type Trampoline a
    = Done a
    | Jump (() -> Trampoline a)


done : a -> Trampoline a
done = Done

jump : (() -> Trampoline a) -> Trampoline a
jump = Jump


evaluate : Trampoline a -> a
evaluate trampoline =
  case trampoline of
    Done value ->      value
    Jump f -> evaluate (f ())

unfoldEv : (b -> Maybe ( b, a )) -> b -> List a
unfoldEv f seed = unfoldInternal f ( seed, Array.empty )|> evaluate

unfoldInternal : (b -> Maybe ( b, a )) -> ( b, Array a ) -> Trampoline (List a)
unfoldInternal f ( seed, accumulator ) =
    case f seed of
        Nothing ->
            done (Array.toList accumulator)

        Just ( newSeed, next ) ->
            unfoldInternal f ( newSeed, Array.push next accumulator )

--mapUnfoldEv : (a -> b) -> List a -> List b 
mapUnfoldEv fct l = 
    let 
        --g :   List a -> Maybe ( b ,  List a  )
        g l0 = 
            case (List.tail l0,List.head l0) of 
                (Just b,Just a) -> Just(b,fct a)
                _ -> Nothing 
    in unfoldEv g  l


