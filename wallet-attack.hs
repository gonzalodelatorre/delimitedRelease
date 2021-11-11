{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}  
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

import DR
import Constructors
import Environment
import InterpreterTest

import qualified Data.Map.Strict               as M

{-

Wallet Attack

- h stores the (secret) amount of money in a customers electronic wallet
- l stores the (public) amount of money spent during the current session  - LOW - 
- k stores the cost of the item to be purchased - LOW -


Γ :
h 0 -> High
l 1 -> Low
k 2 -> Low
n 3 -> Low


Se rompe la integridad de la billetera.

La variable k se está actualizando antes del declassiffy, por eso rompe.


l := 0; 
while (n ≥ 0) do
k := (2 exp n) − 1;
if declassify(h >= k, low) then (h := h − k; l := l + k) else skip;
n := n − 1

-}



-- if  declassify(h >= k, low) then (h := h - k;l := l + k) else Skip

-- entorno de variables con tipos de seguridad
securityEnvironment = (zero, H) :-: (one, L) :-: (two, L) :-: (three, L) :-: (four, L)  :-: Nil


h = var securityEnvironment  zero
l = var securityEnvironment  one
k = var securityEnvironment  two
n = var securityEnvironment  three


lEquals0 = one =: (int 0)
-- k := (2 exp n) − 1;
expOp = ((int 2) ^. n) -. (int 1)


kEqualsExpOp = two =: expOp


--  n >= 0  

nGT0 =  n >. (int 0)

-- n := n − 1
nMinus1 =  n -. (int 1)



nEqualsnMinus1 = three =: nMinus1


-- h := h - k
hMinusk = h -. k


hEqualshMinusk = zero =: hMinusk


-- l := l + k
lPlusk = l +. k

lqualslPlusk = one =: lPlusk


-- (h := h - k;l := l + k)
seqAssigments = hEqualshMinusk \. lqualslPlusk

--  h >= k  

hGtEk = h >=. k


dec_ = declassify hGtEk L


-- if declassify(h >= k, low) then (h := h - k;l := l + k) else Skip
ifStm = iff dec_ seqAssigments skip


{-


l := 0; 
while (n ≥ 0) do
k := (2 exp n) − 1;
if declassify(h >= k, low) then (h := h − k; l := l + k) else skip;
n := n − 1

-}


seq1 = nEqualsnMinus1 \. skip
seq2 = ifStm \. seq1


-- Se rompe:
-- unsecureElectronicWallet  = lEquals0 \. (while nGT0  (kEqualsExpOp \. seq2))


unsecureElectronicWallet  = one =: int 0 \. 
                            (while (n >. int 0)  
							        (two =:  int 2 ^. n -. int 1 \. 
									 iff (declassify (h >=. k) L) 
										    (zero =:  h -. k \. one =:  l +. k)						--seqAssigments 
										    skip \. 
									 three =:  n -. int 1 \. skip))

-- Andan ok. BORAR.
pr1 = skip \. skip
pr2 = iff (h >. k) skip skip
pr3 = while (h >. k) skip 
