import Source.DelimitedRelease
import Source.Constructors
import Source.Environment
import Source.Interpreter

import qualified Data.Map.Strict               as M

{-

Wallet Attack

Writting the program in two different ways just for testing purposes.
Below you will find a better way, less verbose way to write this program.

- h stores the (secret) amount of money in a customers electronic wallet
- l stores the (public) amount of money spent during the current session  - LOW - 
- k stores the cost of the item to be purchased - LOW -


Γ :
h 0 -> High
l 1 -> Low
k 2 -> Low
n 3 -> Low


The integrity of the wallet gets corrupted.
Variable k is beign updated before getting declassified.


l := 0; 
while (n ≥ 0) do
k := (2 exp n) − 1;
if declassify(h >= k, low) then (h := h − k; l := l + k) else skip;
n := n − 1

-}



-- if  declassify(h >= k, low) then (h := h - k;l := l + k) else Skip

-- Security environment for this example.
securityEnvironment = (zero, H) :-: (one, L) :-: (two, L) :-: (three, L) :-: (four, L)  :-: Nil


h = var securityEnvironment zero
l = var securityEnvironment one
k = var securityEnvironment two
n = var securityEnvironment three


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


seq1 = nEqualsnMinus1 \. skip
seq2 = ifStm \. seq1



-- Rejected:
-- unsecureElectronicWallet  = lEquals0 \. (while nGT0  (kEqualsExpOp \. seq2))



{-

This program is rejected.

l := 0; 
while (n ≥ 0) do
k := (2 exp n) − 1;
if declassify(h >= k, low) then (h := h − k; l := l + k) else skip;
n := n − 1

-}



unsecureElectronicWallet  = one =: int 0 \. 
                            (while (n >. int 0)  
							        (two =:  int 2 ^. n -. int 1 \. 
									 iff (declassify (h >=. k) L) 
										    (zero =:  h -. k \. one =:  l +. k)						
										    skip \. 
									 three =:  n -. int 1 \. skip))



          

      


