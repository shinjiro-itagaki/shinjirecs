module Lib where
import Data.Bool(not)

success = (,) True
failed  = (,) False

isSuccess :: (Bool, a) -> Bool
isSuccess = fst

isFailed = not . isSuccess

returnValue :: (Bool, a) -> a
returnValue = snd


(=<<.) :: (Monad m) => (a -> m (Bool, a)) -> (a -> m (Bool, a)) -> (a -> m (Bool, a))
(=<<.) f1 f2 = (\arg -> f1 arg >>= (\res -> let arg2 = returnValue res in if isSuccess res then f2 arg2 else return res))
infixr 8 =<<.

(.>>=) :: (Monad m) => (a -> m (Bool, a)) -> (a -> m (Bool, a)) -> (a -> m (Bool, a))
(.>>=) f1 f2 = f2 =<<. f1
infixl 8 .>>=
  
(<||>) :: (Monad m) =>
           (a -> m (Bool, a)) -- on success
        -> (a -> m a) -- on failed
        -> ((Bool, a) -> m (Bool, a)) -- result
(<||>) onSuccess onFailed =
  \arg ->
    let rtn = returnValue arg in
      if isSuccess arg
      then onSuccess rtn
      else onFailed rtn
           >>= (\rtn2 -> return (False, rtn2))

infix 7 <||>

(.>>||) :: (Monad m) =>
           (a         -> m (Bool, a))
        -> ((Bool, a) -> m (Bool, a))
        -> (a         -> m (Bool, a))
          
(.>>||) f1 f2 = (\arg -> f1 arg >>= f2)
infixl 8 .>>||

(||<<.) :: (Monad m) =>
           (a         -> m (Bool, a))
        -> ((Bool, a) -> m (Bool, a))
        -> (a         -> m (Bool, a))    
(||<<.) f1 f2 = f1 .>>|| f2 

