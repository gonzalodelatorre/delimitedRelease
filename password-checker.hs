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

import DelimitedRelease
import Constructors
import Environment
import Interpreter

import qualified Data.Map.Strict               as M

{-

Password checker

-}


-- Security environment.
securityEnvironment = (zero, H) :-: (one, L) :-: (two, H) :-: (three, H) :-: (four, H) :-: Nil 

-- Database 
database = M.insert 0 23 (M.insert 1 45 (M.insert 2 2391 (M.insert 3 0 (M.insert 4 67 initMemory))))

-- password y userId lo que ingresa el usuario.
password = var securityEnvironment zero
userId = var securityEnvironment one
passwordUserIdHash = var securityEnvironment two
tmp = var securityEnvironment three
newPassword = var securityEnvironment four


-- isMatch = var securityEnvironment three


{-

hash applies buildHash to the password and userId given as input

-}
hash = declassify buildHash L 


{-

buildHash applies Cantor pairing function to the password and userId given as input to build the hash.

password 23
userId 45
buildHash 2391
          
-}
buildHash = ((password +. userId)  *. (password +. userId +. int 1)) // int 2  +. userId

{-

tmpResult expression used for storing temporal results. Used with testing purposes.
          
-}
tmpResult = three  =: buildHash

-- Testing results
-- evalStmWithEnviroment tmpResult database
-- fromList [(0,2),(1,2),(2,2)]


{-

match checks if the password image, or hash, from the database is equal to the hash of 
the user input

returns 0 (False) if there is no match, 1 (True) if it is a match.
          
-}
match = three =: (passwordUserIdHash  =. hash)


-- evalStmWithEnviroment match database
-- fromList [(0,2),(1,2),(2,2)]


{-

update updates the old password hash 
by querying the old password, matching its hash and (if
matched) updating the hashed password with the hash of the password.
          
-}

update = iff (passwordUserIdHash  =. hash)
             (two =: declassify (((newPassword +. userId)  *. (newPassword +. userId +. int 1)) // int 2  +. userId) L) 
             skip


-- evalStmWithEnviroment update database
-- fromList [(0,23),(1,45),(2,6373),(3,0),(4,67)]
-- newPassword on variable two: passwordUserIdHash


{-

attack.

l := 0;
while (n ≥ 0) do
k := 2n−1;
if hash(sign(h − k + 1), 0) = hash(1, 0)
then (h := h − k; l := l + k) else skip;
n := n − 1

Eventualmente voy a descasificar h, y la voy a asignar a una variable low.

          
-}

-- Probamos con otro ambiente.
databaseLaundering =  M.insert 5 23 (M.insert 6 45 (M.insert 7 23391 (M.insert 8 500 (M.insert 9 0 initMemory))))
failedSecurityEnvironment = (five, L) :-: (six, H) :-: (seven, L) :-: (eight, L) :-: (nine, L) :-: Nil

l = var failedSecurityEnvironment five
h = var failedSecurityEnvironment six
k = var failedSecurityEnvironment seven
n = var failedSecurityEnvironment eight
temporalSignal = var failedSecurityEnvironment nine


{-
Couldn't match type ‘'['Succ Five]’ with ‘'[]’
 
attack = five =: int 0 \.
       while (n >. int 0) 
	     (
		 sign  \.
		 seven =:  int 2 ^. (n -. int 1)   \.   
	     (iff     (hashSignal  =. hash10 )       
         	   (six =: h -. k \. five =: l +. k)
 			    skip)
	   
	     \. 

	    eight =:  n -. int 1)

-}


sign = iff (( declassify h L) -. k +. int 1 >. int 0) 
            (nine =: int 1)
			((iff (( declassify h L) -. k +. int 1 <. int 0))
			(nine =: int (-1))
			(nine =: int 0)
			)

hashSignal = ((temporalSignal +. int 0)  *. (temporalSignal +. int 0 +. int 1)) // int 2  +. int 1
hash10 =   ((int 1 +. int 0)  *. (int 1 +. int 0 +. int 1)) // int 2  +. int 1





