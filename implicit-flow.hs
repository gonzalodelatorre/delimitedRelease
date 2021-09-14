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

Implicit Flow

if xH then yL := 1 else skip


Γ :
xH 0 -> High
yL 1 -> Low

No acepta porque si yl es 1, se que xH es true.

-}


type Env = '[ '( 'Zero, 'High),'( One, 'Low) ]


xH = var implicitFlow zero

yL = var implicitFlow one

-- yL := 1
yLEquals1 =  one =: (int 1)

-- No anda, mismo error.
code  =  (iff xH yLEquals1 skip) 