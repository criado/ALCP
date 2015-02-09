module AKS where
#include "Header.hs"

aks::Integer->Bool --isPrime
aks n=
  not(ispow n) &&
  all (\a-> not $ inRange (2,n P.-1) (gcd integer a n) ) [1..r] &&
  (if n<=r then True else 
      all condition [1..floor(sqrt(φ(r))*logBase 2 (n))] )
  where ispow n=
        r= find  
        φ= product . map(\(p,n)->(p-1)*(p^(n-1))) . factor
        condition a= 
          where 
        

