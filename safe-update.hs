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

h1 := 1;
avg := declassify(h2, low)

Este programa anda correctamente porque la variable que se updatea antes de declassify, es distinta a la que aparece
en la escape hatch.

Γ :
l 0 -> Low
h1 1 -> High
h2 2 -> High



-}

-- entorno de variables con tipos de seguridad
securityEnvironment = (zero, L) :-: (one, H) :-: (two, H) :-: Nil

memory = M.insert 0 2 (M.insert 1 2 (M.insert 2 3 initMemory))

l = var securityEnvironment zero

h1 = var securityEnvironment one

h2 = var securityEnvironment two

code = one =: int 1 \.
       zero =:  declassify h2 L
	   
-- evalStmWithEnviroment code memory
-- fromList [(0,3),(1,1),(2,3)]

	
--  Couldn't match type ‘'['Succ One]’ with ‘'[]’
--unsafeUpdate = (two =: (int 1)) \. 
--               (zero =:  declassify h2 L)

