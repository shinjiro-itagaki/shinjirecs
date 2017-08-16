module Class.Object where
class Object o where
  new :: o
  {-
obj = new :: SomeObject
obj ./ getAt (2 * 0) ./ count `div` 10
-}
  (./) :: o -> (o -> a) -> a
  (./) x f = f x
  infixl 9 ./
