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



-- TODO TERMINAR

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
n := n − 1

-}




{-


l := 0; 
while (n ≥ 0) do
k := (2 exp n) − 1;
if declassify(h >= k, low) then (h := h − k; l := l + k) else skip;
n := n − 1

-}

h = var zero
l = var one
k = var two
n = var three

{-
code = (one =: (int 0)) \.
       (while (gt n (int 0))  (    
	   ( two =: minus (exponential (int 2) n) (int 1)   \.   
	   
	    (ifthenelse (declassify h L) skip skip) 
	   
	   ) \. 

	   three =: minus n (int 1) ) )-}
	   
	   
	   
--ode =  --(three =: (int 0)) \.
  --     (while ( (int 0)) skip)  
	   
code = (while ( (int 0)) (two =: (int 3)))

code2 = (while ( gt n (int 0)) (two =: (int 3)) \. skip )


-- No anda, preguntar	 - NO SABE QUE VARS TIENE, PREGUNTAR COMO SOLUCIONARLO  
-- code3 = (while ( (int 0)) skip)
--code3 = (while ( (int 0)) skip \. skip)
code3 = skip \. skip