module Utils.Cmd exposing (..)
(>>=) : Cmd a -> (a -> b) -> Cmd b
(>>=) cmd f = Cmd.map f cmd
infixl 1 >>=

(=<<) f cmd = cmd >>= f
