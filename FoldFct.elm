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

