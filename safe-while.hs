import Source.DelimitedRelease
import Source.Constructors
import Source.Environment
import Source.Interpreter

import qualified Data.Map.Strict               as M

{-

Safe while just tests the while loop in a safety manner.


Γ :
h 0 -> High
l 1 -> Low
k 2 -> Low
n 3 -> Low




l := 0; 
while (n ≥ 0) do
k := (2 exp n) − 1;
n := n − 1

-}




-- Security environment for this example.
securityEnvironment = (zero, H) :-: (one, L) :-: (two, L) :-: (three, L) :-: Nil

memory = M.insert 0 2 (M.insert 1 23 (M.insert 2 3 ((M.insert 3 4 initMemory))))

h = var securityEnvironment zero
l = var securityEnvironment one
k = var securityEnvironment two
n = var securityEnvironment three



code1 = (while ( (int 0)) skip)
code2 = (while ( (int 0)) skip \. skip)
code3 = skip \. skip





safeWhile  = one =: int 0 \. 
                            (while (n >. int 0)  
							        (two =:  int 2 ^. n -. int 1 \. 
									 
									 three =:  n -. int 1 \. skip))


-- Testing the program.
									 
-- evalStmWithEnviroment safeWhile memory			
-- fromList [(0,2),(1,0),(2,1),(3,0)]
						 










