module Experiment where

import Prelude hiding (+) (-) (*) (/) (deg) (div)
import qualified Prelude as P

data Dictionary d=Ring{_zero::d, (.+)::d->d->d}|Group{_one::d, (.*)::d->d->d}

class Structurable d where
  (+)::d->d->Structure d->d
  (*)::d->d->Structure d->d
  zero::Structure d->d
  one::Structure d->d
  literal::Structure d->d->d
  literal=const

data Structure d=ℤ| (Structure d):/d

getOps::Structure d->Dictionary d
  getOps ℤ=

instance Structurable Integer where
  (a+b) ℤ = ((.+) (getOps ℤ)) (a ℤ) (b ℤ)
  (a+b) (s :/ p) = 
