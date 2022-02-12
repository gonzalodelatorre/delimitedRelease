import Source.DelimitedRelease
import Source.Constructors
import Source.Environment
import Source.Interpreter

import qualified Data.Map.Strict               as M

{-

Implicit Flow

if xH then yL := 1 else skip


Γ :
xH 0 -> High
yL 1 -> Low

If yl is 1, then i can discover that xH is true. So it won´t compile.

-}


-- Security environment for this example.
securityEnvironment = (zero, H) :-: (one, L) :-: Nil


xH = var securityEnvironment zero

yL = var securityEnvironment one

-- yL := 1
yLEquals1 =  one =: int 1

{-
If i uncomment this line, the program will get an error:

No instance for (Source.DelimitedRelease.LEq 'High 'Low) arising from a use of ‘iff’

This is correct because it prevents information flow from lower variables 
to higher variables.
-}
--code  =  iff xH yLEquals1 skip