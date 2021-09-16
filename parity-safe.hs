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

No se como emular la función "parity"

Este programa  es seguro porque h no se actualiza antes de desclasificarse.


if declassify(parity(h), low) then (l := 1; h := 1) else (l := 0; h := 0)


Lo voy a cambiar por que no tengo la prosibilidad de escribir funciones (h se updatea en el if)


if declassify( (if mod h 2 eq 0 then h:= 0 else h:= 1; Eq h 1) , low) then (l := 1; h := 1) else (l := 0; h := 0)



Γ :
h 0 -> High
l 1 -> Low


-}


-- entorno de variables con tipos de seguridad
env = (zero, H) :-: (one,L) :-: Nil

h = var env zero

l = var env one


-- if declassify(h = 1, low) then (l := 1; h := 1) else (l := 0; h := 0)
ifStm = iff (declassify (h =. (int 0)) L) ((one  =: (int 1))  \. (zero  =: (int 1))) ((one  =: (int 0))  \. (zero  =: (int 0)))

-- Programa no seguro
-- unsafe =  (zero  =: (int 0)) \. ifStm