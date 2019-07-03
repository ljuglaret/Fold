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

facteurs : (Int,List Int ) -> List Int 
facteurs (x,l) = 
    let 
        g : (Int , List Int ) -> Maybe (Int , (Int , List Int ) ) 
        g (x0 , l0) = 
            case ((x0 ,List.head l0),List.tail l0) of 
                ((x1, Just a),Just b) -> if (modBy  a x1 == 0  )
                                        then Just (a,(x1 , b))
                                        else g (x1,List.drop 1 l0)
                    
                _ -> Nothing 
    in unfold2 g  (x,l) 


facteurs2 xp = facteurs (xp , (List.range 1 xp))



decompositionBase : Int -> Int  ->  List Int 
decompositionBase entier base = 
    let 
        g : (Int , Int ) -> Maybe(Int , (Int , Int ))
        g (entier0,base0)= 
          if (entier0 < 1)
          then Nothing
          else Just(modBy base0 entier0, ( entier0//base0, base0 ))
    in unfold2G g  (entier,base) 



mapUnfold : (a -> b) -> List a -> List b 
mapUnfold fct l = 
    let 
        g :   List a -> Maybe ( b ,  List a  )
        g l0 = 
            case (List.head l0,List.tail l0) of 
                (Just a,Just b) -> Just(fct a,b)
                _ -> Nothing 
    in unfold2 g  l 

filterUnfold : (a -> Bool) -> List a -> List a
filterUnfold  predicat l = 
    let 
        g :   List a -> Maybe ( a ,  List a  )
        g l0 = 
            case (List.head l0,List.tail l0) of 
                (Just a,Just b) -> 
                    if (predicat a ) 
                    then Just(a,b)
                    else g (List.drop 1 l0)
                _ -> Nothing 
    in unfold2  g  l 




filterMapUnfold : (a -> Maybe b ) -> List a -> List b
filterMapUnfold  predicat l = 
    let 
        g :   List a -> Maybe ( b ,  List a  )
        g l0 = 
            case (List.head l0,List.tail l0) of 
                (Just a,Just b) -> 
                    case (predicat a ) of
                        Just p -> Just(p,b)
                        Nothing->g (List.drop 1 l0)
                 
                _ -> Nothing 
    in unfold2  g  l 

