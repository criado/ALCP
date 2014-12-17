module Polynomials where

import Definitions

{-
 - Anillo de polinomios de un cuerpo dado. Se representa un polinomio como una lista de
 - coeficientes, de mayor a menor grado.
 -
 - En adelante, se toma este estandar de identificadores para este archivo:
 - si tenemos un polinomio, a=Pol (a_n:a_i) 
 -   a0 es su término independiente
 -   a_i es la lista de los restantes coeficientes.
 -   a_n es el coeficiente principal.
 -
 - A veces se escribe el polinomio con forma a=Pol a_i
 -  en tal caso, a_i representa toda la lista de coeficientes.
 -
 - Todo esto puede resultar algo confuso, pero se ocultará al usuario de estos polinomios en
 - funciones externas.
 -}

pol::Structure d->Structure [d]
pol field = Euclid {
    _one=[one], _zero=[],
    (.+)=\   p1 p2->(reduction.reverse) $ zipWith (+) (reverse p1) (reverse p2) 
    (.-)= \  p1 p2->p1 .+ (map (-) p2)
    (.*) = \ p1 p2->foldr (map 
    (.==) = \p1 p2->and zipWith (==) (reduction p1) (reduction p2)
    _deg = toInteger.length.reduction
    _division=
  } where Field one zero (+) (-) (*) (==) (/) = field 
          reduction p=dropWhile(zero==)
{-

instance Field f => Group (Pol f) where
 Pol []        *: p2 = Pol []
 Pol (a_0:a_i) *: p2 = 
  reduction $ Pol a_i*:p2 +: (Pol $ (map (a_0*:) $ coefs p2) ++ replicate (length a_i) zero)
 unit = Pol [unit]

instance Field f => Ring (Pol f) where
 Pol a_i +: Pol b_i
  | length a_i >= length b_i = reduction$ Pol $ zipWith (+:) a_i (zeroes ++ b_i)
  | otherwise                = reduction$ Pol b_i +: Pol a_i
  where zeroes = replicate (length a_i-length b_i) zero
 zero = Pol []
 addinverse (Pol a_i) = Pol $ map addinverse a_i

instance Field f => Euclidean (Pol f) where
 deg p = toInteger(length$ coefs$ reduction p) -1
 division a@(Pol a_i) b@(Pol b_i) = (q *: Pol[inverse b_n] , r) where
  b_n=head $ filter (zero/=) b_i
  (q,r)=divmonic (reduction $ a) (reduction $ b *: Pol[inverse b_n])
  divmonic (Pol[]) b = (zero,zero)
  divmonic a@(Pol (a_0:a_i)) b@(Pol b_i)
   | deg a < deg b   = (zero,a)
   | b==zero   = error "division por cero"
   | otherwise = (Pol(a_0:zeroes++coefs q), r)
   where (q,r) = divmonic a' b
         a'    = a -: Pol(b_i++zero:zeroes) *: Pol[a_0]
         zeroes=replicate (fromInteger$if a'==zero then 0 else deg a - deg a' -1) zero
-}
