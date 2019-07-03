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

