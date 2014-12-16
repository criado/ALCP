module Quotient where

import Prelude hiding (mod)
import Definitions
import Control.Exception

mod::Eq d=>Structure d->d->Structure d
mod euclid m=Field{
  _zero=zero,
  _one=one,
  (.+)= \a b-> snd $ division (a+b) m,
  (.-)= \a b-> snd $ division (a-b) m,
  (.*)= \a b-> snd $ division (a*b) m,
  (./)= \a b-> snd $ let (d,s,t)=eea euclid b m in assert (d==one) (a*s)
 } where Euclid one zero (+) (-) (*) deg division=euclid
