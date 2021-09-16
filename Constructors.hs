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

-- Variables
var :: HList env -> SNat (n :: Nat) -> Exp env (Lookup env n) '[] (n ': '[])
var en n = Var n

-- Integer literals
int :: Int -> Exp env 'Low '[] '[]
int = IntLit

-- Boolean literals
bool :: Bool -> Exp env 'Low '[] '[]
bool = BoolLit

-- Operators
x +. y = Ope Plus x y
x -. y = Ope Minus x y
x *. y = Ope Mult x y
x >. y = Ope Gt x y
x >=. y = Ope GtE x y
x <. y = Ope Lt x y
x <=. y = Ope LtE x y
x =. y = Ope Eq x y
x \=. y = Ope NotEq  x y
x ^. y = Ope Exp x y
x // y = Ope Div x y
x %. y = Ope Mod x y 
x &&. y = Ope And x y 
x ||. y = Ope Or x y  

-- Assigment
(=:) n exp = Ass n exp

-- Declassify
declassify :: Exp env l' d vars -> SeType l -> Exp env l vars vars
declassify e l = Declassify e l 

-- Sequence
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

