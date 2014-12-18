module Quotient where

import Prelude hiding (mod)
import Definitions
import Control.Exception

mod::Structure d->d->Structure d
mod euclid m=Field{ --comprobar antes de lanzar esto si m es primo en euclid   
    _zero= zero, _one=one,
    (.==)= \a b-> snd (division (a-b) m)==zero,
    (.+) = \a b-> snd $ division (a+b) m,
    (.-) = \a b-> snd $ division (a-b) m,
    (.*) = \a b-> snd $ division (a*b) m,
    (./) = \a b-> let (d,s,t)=eea euclid b m in assert (d==one) (a*s)
  } where Euclid one zero (==) (+) (-) (*) deg division=euclid

--Los modulos deben ser primos dos a dos (no se comprueba)
--La solución es módulo el producto de todo
chinese :: Structure d-> [(d,d)]->d
chinese euclid = foldr1 chinese2
  where chinese2 (x1,y1) (x2, y2)= x1*y1*(/) (mod euclid y2) one y1 +
                                   x2*y2*(/) (mod euclid y1) one y2 
        Euclid zero one (==) (+) (-) (*) deg division=euclid
