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


Este programa  es seguro porque h no se actualiza antes de desclasificarse.


if declassify(parity(h), low) then (l := 1; h := 1) else (l := 0; h := 0)


Lo voy a cambiar por que no tengo la prosibilidad de escribir funciones (h se updatea en el if)


if declassify( (if mod h 2 eq 0 then h:= 0 else h:= 1; Eq h 1) , low) then (l := 1; h := 1) else (l := 0; h := 0)



Γ :
h 0 -> High
l 1 -> Low


-}

-- Probamos con otro ambiente.
memory = M.insert 0 0 (M.insert 1 32 (M.insert 2 222 (M.insert 3 53 initMemory)))

-- entorno de variables con tipos de seguridad
securityEnvironment = (zero, H) :-: (one,L) :-: Nil

h = var securityEnvironment zero

l = var securityEnvironment one


-- if declassify(h = 1, low) then (l := 1; h := 1) else (l := 0; h := 0)
ifStm =   iff (declassify (h =. int 0) L) 
                  (one  =: int 1  \. zero  =: int 33) 
			      (one  =: int 88  \. zero  =: int 89)
				  
-- evalStmWithEnviroment ifStm memory
-- El resultado depende de la variable 0 que sereo en init enviroment 

-- Programa no seguro
-- unsafe =  (zero  =: (int 0)) \. ifStm