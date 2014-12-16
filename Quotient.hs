module Quotient where

import Definitions
import Control.Exception

quotient'field:: Structure d->d->Structure d
quotient'field euclid m=Field{
  _zero=zero,
  _one=one,
  (.+)= \a b-> snd $ division (a+b) m,
  (.-)= \a b-> snd $ division (a-b) m,
  (.*)= \a b-> snd $ division (a*b) m,
  (./)= \a b-> snd $ let (d,s,t)=eea euclid b m in assert (d==one) (a*s)
 } where one=_one euclid; zero=_zero euclid; (+)=(.+) euclid; (-)=(.-)euclid; (*)=(.*)euclid
         division=_division euclid
