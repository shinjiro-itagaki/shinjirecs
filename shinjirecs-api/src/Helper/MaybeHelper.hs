{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Helper.MaybeHelper where
(||) :: Maybe a -> Maybe a -> Maybe a
(||) (Just x) _        = Just x
(||) Nothing  (Just y) = Just y
(||) Nothing  Nothing  = Nothing

infixl 2 ||
