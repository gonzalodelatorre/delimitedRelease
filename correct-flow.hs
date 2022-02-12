import Source.DelimitedRelease
import Source.Constructors
import Source.Environment
import Source.Interpreter

import qualified Data.Map.Strict               as M



{-

Correct flow.

if yL + 2 then zL := zL + 1 else xH := xH − 1


Γ :
xH 0 -> High
yL 1 -> Low
zL 2 -> Low



-}


-- Security environment for this example.
securityEnvironment = (zero, H) :-: (one, L) :-: (two, L) :-: Nil


xH = var securityEnvironment zero  
yL = var securityEnvironment one 
zL = var securityEnvironment two 

-- xH :=  xH - 1
xHMinusOne =  zero  =: xH -. int 1
-- zL :=  zL - 1
zLPlusOne = two  =: zL  +. int 1



-- Correct If
-- if yL + 2 then zL := zL + 1 else xH := xH − 1
-- ifStm = iff (yL +. (int 2)) zLPlusOne xHMinusOne
-- evalStm ifStm
-- Error thrown because variables got no values stored.
-- *** Exception: Maybe.fromJust: Nothing





ifStm  =   zero =: int 10  \. 
            one =: bool True \.
            two =: int 1 \. 
            iff (yL +. int 2) zLPlusOne xHMinusOne

program  =   zero =: int 0  \.  
             while (xH <. int 10) (zero =: xH +. int 1)



-- evalStm ifStm
-- evalStm ifStm : fromList [(0,9),(1,1),(2,1)]
-- Variables got updated

 