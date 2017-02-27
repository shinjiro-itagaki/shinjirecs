{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Helper where
import Data.Bool(not)
import Data.Maybe(isJust)
import Database.Persist.Class(PersistEntity)
import Data.Time.Clock(UTCTime,addUTCTime,NominalDiffTime)


class ResultClass a r where
  -- please implement
  toResult :: Bool -> a -> r
  
  toSuccess :: a -> r
  toSuccess = toResult True
  toFailed  :: a -> r
  toFailed = toResult False
  
  -- please implement
  returnValue :: r -> a

  -- please implement
--  isSuccess :: r -> a -> Bool
  isSuccess :: r -> a -> Bool
  
  isFailed :: r -> a -> Bool
  isFailed r = not . (isSuccess r)
  
(=<<&&.) :: (Monad m, ResultClass a r) => (a -> m r) -> (a -> m r) -> (a -> m r)
(=<<&&.) f1 f2 = (\arg -> f1 arg >>= (\res -> let arg2 = returnValue res in if isSuccess res arg2 then f2 arg2 else return res))

infixr 8 =<<&&.

(.&&>>=) :: (Monad m, ResultClass a r) => (a -> m r) -> (a -> m r) -> (a -> m r)
(.&&>>=) f1 f2 = f2 =<<&&. f1
infixl 8 .&&>>=

(<||>) :: (Monad m, ResultClass a r) =>
           (a -> m r) -- on success
        -> (a -> m a) -- on failed
        -> (Bool -> a -> m r) -- result
(<||>) onSuccess onFailed =
  \flag rtn ->
  if flag
  then onSuccess rtn
  else onFailed rtn
       >>= (\rtn2 -> return $ toSuccess rtn2)

infix 7 <||>

(.||>>=) :: (Monad m, ResultClass a r) =>
           (a -> m r) -- f1
        -> (Bool -> a -> m r) -- f2 (do success or failed)
        -> (a -> m r)
          
(.||>>=) f1 f2 = (\arg -> f1 arg >>= (\res -> f2 (isSuccess res arg) (returnValue res)))
    
    
infixl 8 .||>>=

(=<<||.) :: (Monad m, ResultClass a r) =>
            (Bool -> a -> m r)
         -> (a -> m r)
         -> (a -> m r)
(=<<||.) f2 f1 = f1 .||>>= f2


(.++) :: UTCTime -> Integer -> UTCTime
(.++) t sec = (fromInteger sec :: NominalDiffTime) `addUTCTime` t
