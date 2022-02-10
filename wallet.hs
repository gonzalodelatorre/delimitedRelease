import Source.DelimitedRelease
import Source.Constructors
import Source.Environment
import Source.Interpreter

import qualified Data.Map.Strict               as M



{-

Wallet

- h stores the (secret) amount of money in a customers electronic wallet
- l stores the (public) amount of money spent during the current session  
- k stores the cost of the item to be purchased 


Γ :
h 0 -> High
l 1 -> Low
k 2 -> Low

Safe:

if  declassify(h >= k, low) then (h := h - k;l := l + k) else Skip

-}


-- Security environment for this example.
securityEnvironment = (zero, H) :-: (one, L) :-: (two, L) :-: Nil


-- if  declassify(h >= k, low) then (h := h - k;l := l + k) else Skip

h = var securityEnvironment zero

l = var securityEnvironment one

k = var securityEnvironment two

-- Testing with a different environment.
memory =  M.insert 0 500 (M.insert 1 0 (M.insert 2 45 initMemory))

secureElectronicWallet = iff (declassify (h  >. k) L)
              
			  (zero =: h -. k  \. one =: l +. k)
		 
		      skip

-- evalStmWithEnviroment secureElectronicWallet memory
-- fromList [(0,455),(1,45),(2,45)]


-- Another example. 
ifStm = iff (declassify (h >. k) L) skip skip
