module Surreals
    ( Surreal(Surreal) ) where

data Surreal = Surreal [Surreal] [Surreal] deriving Show

l (Surreal ls _) = ls
r (Surreal _ rs) = rs

-- x >= y iff no Xᴿ <= y and x <= no yᴸ
x `gte` y = not $ or $ (map (\xr -> xr `lte` y) (r x)) ++ (map (\yl -> x `lte` yl) (l y))
x `lte` y = y `gte` x

instance Eq Surreal where
  x == y = (x `gte` y) && (x `lte` y)

instance Ord Surreal where
  (<=) = lte
