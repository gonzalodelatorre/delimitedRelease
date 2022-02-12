import Source.DelimitedRelease
import Source.Constructors
import Source.Environment
import Source.Interpreter

import qualified Data.Map.Strict               as M

{-

h1 := 1;
avg := declassify(h2, low)


This program works because the variable getting updated before declassificattion occurs, it's different from the one on 
the declassify operation.

Γ :
l 0 -> Low
h1 1 -> High
h2 2 -> High



-}

-- Security environment for this example.
securityEnvironment = (zero, L) :-: (one, H) :-: (two, H) :-: Nil

memory = M.insert 0 2 (M.insert 1 2 (M.insert 2 3 initMemory))

l = var securityEnvironment zero

h1 = var securityEnvironment one

h2 = var securityEnvironment two

code = one =: int 1 \.
       zero =:  declassify h2 L

-- evalStmWithEnviroment code memory
-- fromList [(0,3),(1,1),(2,3)]


{-

This program fails:

Couldn't match type ‘'['Succ One]’ with ‘'[]’

because the variable getting declassified gets updated before actually being declassified.
          
-}

--unsafeUpdate = (two =: (int 1)) \. 
--               (zero =:  declassify h2 L)

