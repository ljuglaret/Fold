module FoldPerso exposing(..)
--import Trampoline exposing (..)
import Array exposing(..)

foldRight : (a -> b -> b) -> b -> List a -> b
foldRight f x l = 
    case l of 
        []      -> x 
        y::s    -> f y (foldRight f x s)

foldLeft : (a -> b -> b) -> b -> List a -> b
foldLeft f x l = 
    case l of 
        [] -> x 
        y::s -> foldLeft  f (f y x )  s


{-
foldR : (a -> b -> b) -> b -> List a -> b
foldR f acc l =
    case  l of 
        [] -> acc
        [a] -> f a acc
        b::a[] -> f b (f a acc)
        e::d::c::b::a[] ->
            let 
                acc5 = f e acc4
                acc4 = f d acc3
                acc3 = f c acc2
                acc2 = f b acc1
                acc1 = f a acc
            in acc5

TO DO LIST
[e,d,c,b,a] 

-}


foldR3 f acc l =   (foldLeft (\elt0 acc0  ->  (f elt0 acc0))) (identity l) acc

unfold2 : (a -> Maybe(b,a))   -> a -> List b 
unfold2      f              state  =
            case f state of
                    Just (x, newState) -> 
                
                         x:: unfold2  f  newState
                    Nothing            -> []



unfold2G : (a -> Maybe(b,a))   -> a -> List b 
unfold2G      f              state  =
            case f state of
                    Just (x, newState) -> 
                
                        (unfold2G  f  newState) ++ [x]
                    Nothing            -> []

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
