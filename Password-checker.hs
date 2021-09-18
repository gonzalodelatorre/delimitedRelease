{-# LANGUAGE DataKinds #-}
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

Password checking

-}


-- entorno de variables con tipos de seguridad
env = (zero, H) :-: (one, L) :-: (two, L) :-: (three, H) :-: (four, L)  :-: Nil



-- Probamos con otro ambiente.
database =  M.insert 0 23 (M.insert 1 45 (M.insert 2 23391 (M.insert 3 0 initEnv)))


password = var env zero
userId = var env one
passwordImage = var env two

tmp = var env three


-- isMatch = var env three

hash = declassify buildHash L 


-- Cantor pairing function for hashing two numbers
buildHash =   ((password +. userId)  *. (password +. userId +. int 1)) // int 2  +. userId



--password 23
--userId 45
-- buildHash 2391
--- tmpResult = three  =: buildHash


-- Matching the user input to the password image from the database
-- TODO RENAME THIS ASS PASSWORD CHECKER
match = three =: (passwordImage  =. hash)
-- False 0, True 1

--update = iff match(pwdImg, salt, oldPwd)
--        then passwordImage := hash(newPwd, salt)
-- else skip


-- evalStmWithEnviroment match database
-- fromList [(0,2),(1,2),(2,2)]