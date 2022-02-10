import Source.DelimitedRelease
import Source.Constructors
import Source.Environment
import Source.Interpreter

import qualified Data.Map.Strict               as M





{-

Password checker attack.

l := 0;
while (n = 0) do
k := 2n-1;
if hash(sign(h - k + 1), 0) = hash(1, 0)
then (h := h - k; l := l + k) else skip;
n := n - 1

Eventually h will get declassified and later assigned to a low variable.

          
-}





-- Database 
databaseLaundering =  M.insert 0 23 (M.insert 1 45 (M.insert 2 23391 (M.insert 3 500 (M.insert 4 0 initMemory))))

-- Security environment.
failedSecurityEnvironment = (zero, L) :-: (one, H) :-: (two, L) :-: (three, L) :-: (four, L) :-: Nil

l = var failedSecurityEnvironment zero
h = var failedSecurityEnvironment one
k = var failedSecurityEnvironment two
n = var failedSecurityEnvironment three
temporalSignal = var failedSecurityEnvironment four



-- Couldn't match type: '[ 'Succ 'Zero]

{-
 
passwordAttack = zero =: int 0 \.
       while (n >. int 0) 
	     (
		 sign  \.
		 two =:  int 2 ^. (n -. int 1)   \.   
	     (iff (hashSignal  =. hash10 )       
         	    (one =: h -. k \. zero =: l +. k)
 			    skip)
	    \. 
        three =:  n -. int 1
		)

-}


sign = iff ((declassify h L) -. k +. int 1 >. int 0) 
            (four =: int 1)
			((iff (( declassify h L) -. k +. int 1 <. int 0))
			(four =: int (-1))
			(four =: int 0)
			)

hashSignal = ((temporalSignal +. int 0)  *. (temporalSignal +. int 0 +. int 1)) //. int 2  +. int 1
hash10 =   ((int 1 +. int 0)  *. (int 1 +. int 0 +. int 1)) //. int 2  +. int 1





