module Numbers where

import Definitions

instance Group Integer where
 (*:) = (*)
 unit = 1
instance Ring Integer where
 (+:) = (+)
 zero = 0
 addinverse x = -x
instance Euclidean Integer where
 deg n|n==0      = -1
      |otherwise = 1+deg(quot n 2)
 division a b = quotRem a b

instance Group Double where
 (*:) = (*)
 unit = 1.0
instance Ring Double where
 (+:) = (+)
 zero = 0.0
 addinverse x = -x
instance Field Double where
 inverse = (1.0/) 
