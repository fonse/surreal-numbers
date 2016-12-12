module Surreals
    ( Surreal(N) ) where

data Surreal = N [Surreal] [Surreal] deriving Show

l (N ls _) = ls
r (N _ rs) = rs

-- x >= y iff no Xᴿ <= y and x <= no yᴸ
x `gte` y = not $ or $ (map (\xr -> xr `lte` y) (r x)) ++ (map (\yl -> x `lte` yl) (l y))
x `lte` y = y `gte` x

instance Eq Surreal where
  x == y = (x `gte` y) && (x `lte` y)

instance Ord Surreal where
  (<=) = lte
