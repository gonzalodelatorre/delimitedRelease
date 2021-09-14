{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}  
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE UndecidableInstances #-} 

module Constructors where

import DR
import Environment


var :: HList env -> SNat (n :: Nat) -> Exp env (Lookup env n) '[] (n ': '[])
var en n = Var n


int :: Int -> Exp env 'Low '[] '[]
int = IntLit

bool :: Bool -> Exp env 'Low '[] '[]
bool = BoolLit

plus = Ope Plus
minus = Ope Minus
mult = Ope Mult
gt   = Ope Gt 
gte   = Ope GtE 
lt   = Ope Lt 
lte   = Ope LtE 
eq = Ope Eq
neq = Ope NotEq
expo = Ope Exp -- TODO CAMBIAR NOMBRE
(//) = Ope Div 
(%.) = Ope Mod
(&.) = Ope And 
(|.) = Ope Or 

(=:) n exp = Ass n exp

declassify :: Exp env l' d vars -> SeType l -> Exp env l vars vars
declassify e l = Declassify e l 


(\.)
  :: (Intersection u1 d2 ~ '[]) =>
     Stm env pc u1 d1
     -> Stm env pc' u2 d2
     -> Stm env (Meet pc pc') (Union u1 u2) (Union d1 d2)
(\.) c1 c2 = Seq c1 c2

-- Skip
skip = Skip

-- If
iff c e1 e2 = If c e1 e2

-- While
while c e1 = While c e1


