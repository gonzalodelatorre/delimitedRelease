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

import DR
import Constructors
import Environment
import InterpreterTest

import qualified Data.Map.Strict               as M



{-

Flow Correcto

if yL + 2 then zL := zL + 1 else xH := xH − 1


Γ :
xH 0 -> High
yL 1 -> Low
zL 2 -> Low



-}


-- entorno de variables con tipos de seguridad
env = (zero, H) :-: (one, L) :-: (two, L) :-: Nil


xH = var env zero  
yL = var env one 
zL = var env two 

-- xH :=  xH - 1
xHMinusOne =  zero  =: xH -. int 1
-- zL :=  zL - 1
zLPlusOne = two  =: zL  +. int 1



-- Correct If
-- if yL + 2 then zL := zL + 1 else xH := xH − 1
-- ifStm = iff (plus yL (int 2)) zLPlusOne xHMinusOne
-- Tira error porque las variables no tienen valores.



ifStm2  =   zero =: int 10  \. 
            one =: bool True \.
            two =: int 1 \. 
            iff (yL +. int 2) zLPlusOne xHMinusOne

program  =   zero =: int 0  \.  
             while (xH <. int 10) (zero =: xH +. int 1)



-- evalStm ifStm2
-- evalStm ifStm2 : fromList [(0,9),(1,1),(2,1)]
-- Las variables se actualizan

 