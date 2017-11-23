{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Helper where
import Data.Bits(shiftL,(.&.),(.|.),Bits)
import Data.Bool(Bool,not)
import Data.Maybe(isJust)
import Database.Persist.Class(PersistEntity)
import Data.Text(replace,pack,unpack)
import Data.Word(Word)
import Control.Monad.IO.Class(MonadIO) -- base
import System.Process(CreateProcess,createProcess,proc)
import Class.Castable
import Control.Monad(Monad) -- base
import Data.Either(Either(..))

data OnResultFunc m a = OnSuccessFunc (a -> m (Bool,a))
                      | OnFailedFunc  (a -> m a)
                      | OnEitherFunc  (a -> m (Bool,a)) (a -> m a)

(>>==) :: (Monad m) => m (Bool,a) -> OnResultFunc m a -> m (Bool,a)
(>>==) mRes (OnSuccessFunc fs) = do
  res <- mRes
  case res of
    (True, v) -> fs v
    (False,v) -> mRes

(>>==) mRes (OnFailedFunc ff) = do
  res <- mRes
  case res of
    (True, v) -> mRes
    (False,v) -> ff v >>= return . (,) False

(>>==) mRes (OnEitherFunc fs ff) = do
  res <- mRes
  case res of
    (True ,v) -> mRes >>== (OnSuccessFunc fs)
    (False,v) -> mRes >>== (OnFailedFunc  ff)

infixl 2 >>==

{-
(./) :: a -> (a -> b) -> b
(./) arg func = func arg
-}
