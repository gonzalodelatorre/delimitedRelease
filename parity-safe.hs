import Source.DelimitedRelease
import Source.Constructors
import Source.Environment
import Source.Interpreter

import qualified Data.Map.Strict               as M

{-


This program is safe because h does not get updated before it gets declassified.


if declassify(parity(h), low) then (l := 1; h := 1) else (l := 0; h := 0)


I changed the program a little bit because the language itself does not provide a way to writte functions.


if declassify( (if mod h 2 eq 0 then h:= 0 else h:= 1; Eq h 1) , low) then (l := 1; h := 1) else (l := 0; h := 0)



Γ :
h 0 -> High
l 1 -> Low


-}

-- Testing the program with different environment.
memory = M.insert 0 0 (M.insert 1 32 (M.insert 2 222 (M.insert 3 53 initMemory)))

-- Security environment for this example.
securityEnvironment = (zero, H) :-: (one,L) :-: Nil

h = var securityEnvironment zero

l = var securityEnvironment one


-- if declassify(h = 1, low) then (l := 1; h := 1) else (l := 0; h := 0)
ifStm =   iff (declassify (h =. int 0) L) 
                  (one  =: int 1  \. zero  =: int 33) 
                  (one  =: int 88  \. zero  =: int 89)

-- evalStmWithEnviroment ifStm memory
-- The result depends on the variable 0.

{-
Unsecure program

Couldn't match type: '[ 'Zero]
                     with: '[]


-}
-- unsafe =  (zero  =: (int 0)) \. ifStm


{-
Unsecure program


This is the final example of section 4 (Sabenfeld and Myers).
Which shows that even if this program is secure (because it only leaks the parity of h
and not the whole value of h)it doesn´t pass the type checker.
 

-}
--unsafe2 = (zero  =: h %. (int 2) =. int 0 ) \.
--          (one  =: (declassify h L))





