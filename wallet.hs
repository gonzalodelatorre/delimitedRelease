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


-- entorno de variables con tipos de seguridad
env = (zero, H) :-: (one, L) :-: (two, L) :-: Nil


-- if  declassify(h >= k, low) then (h := h - k;l := l + k) else Skip

h = var env zero

l = var env one

k = var env two

-- Probamos con otro ambiente.
initEnv2 =  M.insert 0 2 (M.insert 1 2 (M.insert 2 2 initEnv))

ifStm = iff (declassify (h  >. k) L) ( (zero =: (h -. k))  \. (one =: (l +. k))) skip

-- evalStmWithEnviroment ifStm initEnv2
-- fromList [(0,2),(1,2),(2,2)]


-- 
ifStm2 = iff (declassify (h >. k) L) skip skip
