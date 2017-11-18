module Utils.Maybe exposing (..)
                
or : Maybe a -> Maybe a -> Maybe a
or x y = case x of
             Just _ -> x
             Nothing -> y

(<|>) a b = or a b
infixl 1 <|>
                        
catMaybes : List (Maybe a) -> List a
catMaybes xs = case xs of
                   []            -> []
                   (Just x)::xs_ -> x :: (catMaybes xs_)
                   Nothing ::xs_ -> catMaybes xs_
                     
