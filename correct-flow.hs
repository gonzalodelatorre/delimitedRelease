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

Flow Correcto

if yL + 2 then zL := zL + 1 else xH := xH − 1


Γ :
xH 0 -> High
yL 1 -> Low
zL 2 -> Low



-}


xH = var correctFlow zero  
yL = var correctFlow one 
zL = var correctFlow two 

-- xH :=  xH - 1
xHMinusOne =  zero  =: (minus xH (int 1)) 
-- zL :=  zL - 1
zLPlusOne = two  =: (plus zL (int 1))



-- Correct If
-- if yL + 2 then zL := zL + 1 else xH := xH − 1
-- ifStm = iff (plus yL (int 2)) zLPlusOne xHMinusOne
-- Tira error porque las variables no tienen valores.



ifStm2  =  ( zero =: (int 10)  \. ((one =: (bool True)) \. (two =: (int 1))) ) \. (iff (plus yL (int 2)) zLPlusOne xHMinusOne)

-- program  =   (zero =: (int 0))  \.  (while (lt xH (int 10)) (zero =: (plus xH (int 1))))



-- evalStm ifStm2
-- evalStm ifStm2 : fromList [(0,9),(1,1),(2,1)]
-- Las variables se actualizan

 