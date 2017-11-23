{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Helper.NumHelper where
import Text.Printf(printf,PrintfType)

pNum0xd :: Word -> Word -> String
pNum0xd 0    x = pNum0xd 1 x
pNum0xd keta x = printf ("%0" ++ (show keta) ++ "d") x
