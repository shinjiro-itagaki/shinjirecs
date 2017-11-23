{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Helper.ListHelper where
import Data.List(group,elem)
delete :: (Eq a) => [a] -> [a] -> [a]
delete []     all = all
delete (x:xs) all = delete xs $ filter (not . (==x)) all

(\\) all dellist = delete dellist all
infixl 6 \\
  
uniq :: (Eq a) => [a] -> [a]
uniq xs = impl' [] xs
  where
--    impl' :: (Eq a) => [a] -> [a] -> [a]
    impl' rtn [] = rtn
    impl' rtn list@(x:xs) =
      if elem x rtn
      then impl' rtn          xs
      else impl' (rtn ++ [x]) xs

    
