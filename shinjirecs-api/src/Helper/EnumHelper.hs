{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Helper.EnumHelper where
import Data.Bits(shiftL,(.&.),(.|.),Bits)

all ::(Enum a, Bounded a) => [a]
all = [minBound .. maxBound]

enumToMask :: (Enum a, Num b, Bits b) => a -> b
enumToMask x = fromInteger $ 1 `shiftL` (fromEnum x)
