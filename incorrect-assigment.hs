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

Incorrect assigment

vl := vh 

Γ :
vl 0 -> Low
vh 1 -> High

-}

-- entorno de variables con tipos de seguridad
securityEnvironment = (zero, L) :-: (one, H) :-: Nil


vl = var securityEnvironment zero
vh = var securityEnvironment one


-- vl := vh 
code = zero =: vh