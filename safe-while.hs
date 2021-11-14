{-# LANGUAGE DataKinds #-}
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



import Source.DelimitedRelease
import Source.Constructors
import Source.Environment
import Source.Interpreter

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




-- entorno de variables con tipos de seguridad
securityEnvironment = (zero, H) :-: (one, L) :-: (two, L) :-: (three, L) :-: Nil

memory = M.insert 0 2 (M.insert 1 23 (M.insert 2 3 ((M.insert 3 4 initMemory))))

h = var securityEnvironment zero
l = var securityEnvironment one
k = var securityEnvironment two
n = var securityEnvironment three



code1 = (while ( (int 0)) skip)
code2 = (while ( (int 0)) skip \. skip)
code3 = skip \. skip





safeWhile  = one =: int 0 \. 
                            (while (n >. int 0)  
							        (two =:  int 2 ^. n -. int 1 \. 
									 
									 three =:  n -. int 1 \. skip))

									 
--  evalStmWithEnviroment safeWhile memory									 










