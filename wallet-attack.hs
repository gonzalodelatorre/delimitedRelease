{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}  
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- Para que las clases puedan tener más de un parámetro
{-# LANGUAGE FlexibleInstances #-} -- Para usar variables en las instancias de clases
{-# LANGUAGE FlexibleContexts #-} -- Lo Agregué
{-# LANGUAGE UndecidableInstances #-} -- Lo Agregué
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

h = var walletAttack zero
l = var walletAttack one
k = var walletAttack two
n = var walletAttack three


lEquals0 = one =: (int 0)
-- k := (2 exp n) − 1;
expOp = minus ( expo (int 2) n) (int 1)


kEqualsExpOp = two =: expOp


--  n >= 0  

nGT0 = gt n (int 0)

-- n := n − 1
nMinus1 =  minus n (int 1)



nEqualsnMinus1 = three =: nMinus1


-- h := h - k
hMinusk = minus h k


hEqualshMinusk = zero =: hMinusk


-- l := l + k
lPlusk = plus l k

lqualslPlusk = one =: lPlusk


-- (h := h - k;l := l + k)
seqAssigments = hEqualshMinusk \. lqualslPlusk

--  h >= k  

hGtEk = gt h k


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
code = lEquals0 \. (while nGT0  (kEqualsExpOp \. seq2))

-- Andan ok. BORAR.
pr1 = skip \. skip
pr2 = iff (gt h k) skip skip
pr3 = while (gt h k) skip 
