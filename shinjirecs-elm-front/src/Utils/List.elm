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

find : (a -> Bool) -> List a -> Maybe a
find f list =
    case list of
        []     -> Nothing
        (x::xs) -> if f x then Just x else find f xs

deleteIf : (a -> Bool) -> List a -> List a
deleteIf f list =
    case list of
        []      -> []
        (x::xs) -> if f x then deleteIf f xs else x :: (deleteIf f xs)

fromMaybe : Maybe (List a) -> List a
fromMaybe mxs = case mxs of
                    Just xs -> xs
                    Nothing -> []
