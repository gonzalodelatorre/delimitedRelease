import Source.DelimitedRelease
import Source.Constructors
import Source.Environment
import Source.Interpreter

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

-- Security environment for this example.
securityEnvironment = (zero, L) :-: (one, H) :-: (two, H) :-: (three, H) :-: (four, H) :-: Nil


-- Initial Memory.
memory = M.insert 0 2 (M.insert 1 10 (M.insert 2 3 (M.insert 3 2 initMemory)))

avg = var securityEnvironment zero
h1 = var securityEnvironment one  
h2 = var securityEnvironment two 
h3 = var securityEnvironment three 


-- Correct evaluation.
averageSalaries = zero =: declassify ((h1 +. h2 +. h3) //. int 3) L
-- evalStmWithEnviroment averageSalaries memory
-- fromList [(0,5),(1,10),(2,3),(3,2)]
-- Variables 0 changes as the values on the environment changes.


-- Works
precedence = four =: (h1 +. h2 +. h3) //. int 3 
-- evalStmWithEnviroment precedence memory
-- fromList [(0,2),(1,10),(2,3),(3,2),(4,5)]


-- If i put declassify averege H, i won't compile.



-- Won´t work. Example given in the thesis.
{-
unsecureProgram = one =: int 100 \.   -- Swapping the values h1, h2.
                  two =: h1 \.
                  three =: h1 \.
                  averageSalaries 
-}

-- Assigment example.
assigmentExample = one =: int 100
-- *Main> evalStmWithEnviroment assigmentExample memory
-- fromList [(0,2),(1,100),(2,3),(3,2)]


-- Calling evalStmWithEnviroment precedence initMemory will return
-- fromList *** Exception: Maybe.fromJust: Nothing

