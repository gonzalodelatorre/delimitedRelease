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

import DR
import Constructors
import Environment
import InterpreterTest

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