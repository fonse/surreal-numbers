module Surreals
    ( Surreal(Surreal) ) where

data Surreal = Surreal [Surreal] [Surreal] deriving Show

leftSet  (Surreal ls _) = ls
rightSet (Surreal _ rs) = rs

-- x >= y iff no Xᴿ <= y and x <= no yᴸ
x `gte` y = not $ or $ (map (\xr -> xr `lte` y) (rightSet x)) ++ (map (\yl -> x `lte` yl) (leftSet y))
x `lte` y = y `gte` x

instance Eq Surreal where
  x == y = (x `gte` y) && (x `lte` y)

instance Ord Surreal where
  (<=) = lte

instance Num Surreal where
  x + y = Surreal left right
    where
      left  = map (+y) (leftSet x) ++ map (+x) (leftSet y)
      right = map (+y) (rightSet x) ++ map (+x) (rightSet y)

  x * y = Surreal (left1++left2) (right1++right2)
    where
      left1   = [ xl*y + x*yl - xl*yl | xl <- leftSet x,  yl <- leftSet y  ]
      left2   = [ xr*y + x*yr - xr*yr | xr <- rightSet x, yr <- rightSet y ]
      right1  = [ xl*y + x*yr - xl*yr | xl <- leftSet x,  yr <- rightSet y ]
      right2  = [ xr*y + x*yl - xr*yl | xr <- rightSet x, yl <- leftSet y  ]

  negate (Surreal ls rs) = Surreal (map negate rs) (map negate ls)
  abs x = error "Not yet implemented"
  signum x = error "Not yet implemented"
  fromInteger n = error "Not yet implemented"
