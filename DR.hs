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


module DR where

import Control.Monad.Reader
----------------------------
-- values promoted to types

data Nat = Zero | Succ Nat
data SType = Low | High
type Top = High


type family Elem (x :: a) (xs :: [a]) :: Bool where
  Elem x '[]       = 'False
  Elem x (x ': xs) = 'True 
  Elem x (y ': xs) = Elem x xs 

type family IfThenElse (b :: Bool) (t :: a) (u :: a) :: a where
   IfThenElse 'True  t u = t
   IfThenElse 'False t u = u 

-- append
type family Union (xs :: [a]) (ys :: [a]):: [a] where 
    Union '[]       ys = ys
    Union (x ': xs) ys = x ': (Union xs ys) 

type family Intersection (xs :: [a]) (ys :: [a]) :: [a] where
  Intersection '[] ys = '[]
  Intersection xs '[] = '[]
  Intersection (x ': xs) ys = IfThenElse (Elem x ys) 
                                         (x ': Intersection xs ys) 
                                         (Intersection xs ys)

type family Lookup (env :: [(k,st)]) (n :: k) :: a where 
    Lookup ('(n,st) ': env) n = st 
    Lookup ('(m,st) ': env) n = Lookup env n   

-- Max
type family Join (st :: SType) (st' :: SType) :: SType where  
   Join 'Low  x = x
   Join 'High x = 'High

class LEq (a :: SType) (b :: SType) 
instance LEq 'Low x 
instance LEq 'High 'High


-- Min
type family Meet (st :: SType) (st' :: SType) :: SType where 
   Meet 'Low  x    = 'Low
   Meet 'High x    = x
   Meet x    'High = x
   Meet x    'Low  = 'Low

data Op = Plus | Minus| Mult | Div | Exp | Mod 
        | And | Or | Gt | GtE | Lt | LtE | Eq | NotEq 

{-
-- singleton type para los naturales
-- por cada natural en el type-level hay una copia en el value-level 
-- por ej. SSucc SZero :: SNat (Succ Zero)
-}
data SNat (n :: Nat) where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)


-- por cada natural en el type-level hay una copia en el value-level 
-- por ej. SSucc SZero :: SNat (Succ Zero)

data SeType (s :: SType ) where
  L :: SeType 'Low
  H :: SeType 'High



data Proxy (a :: k) = Proxy  

data Exp :: [(Nat,SType)] -> SType -> [Nat] -> [Nat] -> * where
    Var :: SNat (n :: Nat) -> Exp env (Lookup env n) '[] '[n]
   -- Val :: a -> Exp env Low '[] '[] -- Se podría agregar para aumentar expresividad? 
    -- como las operaciones (tipo Op) son enteras y booleanas se podria
    -- agregar tambien literales booleanos, pero eso va a complejizar el
    -- lenguaje dado que habria que chequear el tipo de las expresiones respecto 
    -- a los valores que manipulan (ademas de chequear el tipo de seguridad) 
    IntLit :: Int -> Exp env 'Low '[] '[] 
    BoolLit :: Bool -> Exp env 'Low '[] '[] 
    Ope :: Op -> 
           Exp env st d var1 -> 
           Exp env st' d' var2 -> 
           Exp env (Join st st') (Union d d') (Union var1 var2)
           
    Declassify :: Exp env l' d vars -> SeType l -> Exp env l vars vars 


-- Stm env pc u d
data Stm :: [(Nat,SType)] -> SType -> [Nat] -> [Nat] -> * where

 Skip :: Stm env 'High '[] '[] -- TODO CAMBIO ESTO
 
 Ass :: LEq st (Lookup env n) => 
        SNat (n :: Nat) -> 
        Exp env st d var ->  
        Stm env (Lookup env n) '[n] d 

 Seq :: Intersection u1 d2 ~ '[] =>   -- IsEmpty 
        Stm env pc u1 d1 -> 
        Stm env pc' u2 d2 -> 
        Stm env (Meet pc pc') (Union u1 u2) (Union d1 d2)

-- yo recomendaria trabajar con ifzero en lugar de con un if
-- cuya condicion sea una expresion booleana  
 If  :: LEq st (Meet pc pc') =>  
        Exp env st d vars -> -- para ifzero pondria Var n
        Stm env  pc u1 d1 ->
        Stm env  pc' u2 d2 -> 
        Stm env (Meet pc pc') (Union u1 u2) (Union d (Union d1 d2)) 

-- idem que para if, usaria una variable como condicion en lugar
-- de una expresin booleana para la condicion
 While :: (Intersection u1 (Union d d1) ~ '[], LEq st pc) =>  -- TODO CAMBIO ESTO
          Exp env st d vars -> 
          Stm env pc u1 d1 ->
          Stm env pc u1 (Union d d1) --error en variables  

     
