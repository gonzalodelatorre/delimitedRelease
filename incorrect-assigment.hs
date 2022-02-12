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

-- Security environment for this example.
securityEnvironment = (zero, L) :-: (one, H) :-: Nil


vl = var securityEnvironment zero
vh = var securityEnvironment one



{-
If i uncomment this line, the program will get an error:

No instance for (Source.DelimitedRelease.LEq 'High 'Low) arising from a use of ‘=:’

This is correct because it prevents a direct assigment from lower variables 
to higher variables.
-}
-- vl := vh 
--code = zero =: vh