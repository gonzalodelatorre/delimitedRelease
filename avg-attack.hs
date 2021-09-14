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

Avg - Attack

Variables h1, h2, h3 store the salaries of the three employees.
Variable avg is intendend to intentionally release the average but no other information
about h1, h2 nd h3.

Clearly, this program does not satisfy noninterference.


h1 := 1; h2 := h1; h3 := h1;
avg := declassify((h1 + · · · + hn)/n, low)


Γ :
avg 0 -> Low
h1 1 -> High
h2 2 -> High
h3 3 -> High
	


-}


-- Probamos con otro ambiente.
initEnvironment = M.insert 0 2 (M.insert 1 8 (M.insert 2 22 (M.insert 3 5 initEnv)))

avg =  var avgAttack zero
h1 =  var avgAttack one  
h2 =  var avgAttack two 
h3 =  var avgAttack three 


-- (h1+h2+h3)/n
average = (plus h1 (plus h2  h3)) // (int 3)

-- Este programa es correcto.
-- La evaluacion es correcta.
averageSalaries = zero =: declassify ((plus h1 (plus h2  h3)) // (int 3)) L

-- Observacion, si pongo declassify average H NO Tipa

-- evalStmWithEnviroment averageSalaries initEnvironment
-- fromList [(0,11),(1,8),(2,22),(3,5)]
-- La variable 0 va cambiando, si cambio los valores del ambiente


-- Rompe
-- No tipa, este es el ejemplo que pongo en la tesis.
--code2 =   (one =: (int 10)) \.   -- -- Swapping the values h1, h2.
--         (two =: h1) \.
--        (three =: (int 0)) \.
--         averageSalaries 


-- Ejemplo de asignacion
-- code2 =   (one =: (int 100))
-- *Main> evalStmWithEnviroment code2 initEnvironment
-- fromList [(0,2),(1,10),(2,6000),(3,3000)]