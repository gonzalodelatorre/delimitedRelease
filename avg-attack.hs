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

-- Entorno de seguridad.
securityEnvironment = (zero, L) :-: (one, H) :-: (two, H) :-: (three, H) :-: (four, H) :-: Nil


-- Memoria incial.
initMemory = M.insert 0 2 (M.insert 1 3000 (M.insert 2 6000 (M.insert 3 3000 initEnv)))

avg =  var securityEnvironment zero
h1 =  var securityEnvironment one  
h2 =  var securityEnvironment two 
h3 =  var securityEnvironment three 


-- Habría que darle menos precedencia a =: que a los operadores aritméticos, para no
-- escribir tantos paréntesis, por ejemplo, que tipe ésto: four =: (h1 +. h2 +. h3) // (int 3) 
-- también se deberían poder sacar los paréntesis en int, teniendo éste la mayor precedencia 


-- Este programa es correcto.
-- La evaluacion es correcta.
averageSalaries = zero =: declassify ((h1 +. h2 +. h3) // int 3) L
-- evalStmWithEnviroment averageSalaries initMemory
-- fromList [(0,11),(1,8),(2,22),(3,5)]
-- La variable 0 va cambiando, si cambio los valores del ambiente


-- Works
precedence = four =: (h1 +. h2 +. h3) // int 3 
-- evalStmWithEnviroment precedence initMemory
-- fromList [(0,2),(1,8),(2,22),(3,5),(4,11)]


-- a lo mejor hay una manera para poner avg en lugar de zero que se aproxima 
-- más a un lenguaje imperativo.





-- Observacion, si pongo declassify average H NO Tipa



-- No tipa, este es el ejemplo que pongo en la tesis.

unsecureProgram = one =: int 10 \.   -- Swapping the values h1, h2.
                  two =: h1 \.
                  three =: h1 \.
                  averageSalaries 


-- Ejemplo de asignación.
assigmentExample = one =: int 100
-- *Main> evalStmWithEnviroment assigmentExample initMemory
-- fromList [(0,2),(1,100),(2,22),(3,5)]