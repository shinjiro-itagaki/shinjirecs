module Utils.List exposing (..)

mapWithIndex : (a -> Int -> b) -> List a -> List b
mapWithIndex f list =
    let
        impl : List a -> Int -> List b
        impl list_ idx =
            case list_ of
                []   -> []
                x::[] -> [f x idx]
                x::xs -> (impl (List.singleton x) idx) ++ (impl xs (idx + 1))
    in
        impl list 0
