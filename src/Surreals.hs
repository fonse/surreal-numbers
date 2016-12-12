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

instance Num Surreal where
  x + y = Surreal left right
    where
      left  = map (+y) (l x) ++ map (+x) (l y)
      right = map (+y) (r x) ++ map (+x) (r y)

  x * y = Surreal (left1++left2) (right1++right2)
    where
      left1  = [xl*y + x*yl - xl*yl | xl <- l x, yl <- l y]
      left2  = [xr*y + x*yr - xr*yr | xr <- r x, yr <- r y]
      right1  = [xl*y + x*yr - xl*yr | xl <- l x, yr <- r y]
      right2  = [xr*y + x*yl - xr*yl | xr <- r x, yl <- l y]

  negate (Surreal ls rs) = Surreal (map negate rs) (map negate ls)
  abs x = error "Not yet implemented"
  signum x = error "Not yet implemented"
  fromInteger n = error "Not yet implemented"
