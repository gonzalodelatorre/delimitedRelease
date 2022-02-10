{-# LANGUAGE DataKinds,
			 GADTs, 
			 TypeFamilies,
		     PolyKinds, 
		     TypeOperators,
			 MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}



module Source.DelimitedRelease where



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

-- Append
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

Singleton type for natural numbers

-}
data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)



{-

Singleton type for security levels

-}
data SeType (s :: SType) where
  L :: SeType 'Low
  H :: SeType 'High



{-

Expressions

-}
data Exp :: [(Nat,SType)] -> SType -> [Nat] -> [Nat] -> * where
    Var :: SNat (n :: Nat) -> Exp env (Lookup env n) '[] '[n]
    IntLit :: Int -> Exp env 'Low '[] '[] 
    BoolLit :: Bool -> Exp env 'Low '[] '[] 
    Ope :: Op -> 
           Exp env st d var1 -> 
           Exp env st' d' var2 -> 
           Exp env (Join st st') (Union d d') (Union var1 var2)
           
    Declassify :: Exp env l' d vars -> SeType l -> Exp env l vars vars 


{-

Commands

-}
data Stm :: [(Nat,SType)] -> SType -> [Nat] -> [Nat] -> * where

 Skip :: Stm env 'High '[] '[] 
 
 Ass :: LEq st (Lookup env n) => 
        SNat (n :: Nat) -> 
        Exp env st d var ->  
        Stm env (Lookup env n) '[n] d 

 Seq :: Intersection u1 d2 ~ '[] =>   
        Stm env pc u1 d1 -> 
        Stm env pc' u2 d2 -> 
        Stm env (Meet pc pc') (Union u1 u2) (Union d1 d2)

 If  :: LEq st (Meet pc pc') =>  
        Exp env st d vars -> 
        Stm env  pc u1 d1 ->
        Stm env  pc' u2 d2 -> 
        Stm env (Meet pc pc') (Union u1 u2) (Union d (Union d1 d2)) 

 While :: (Intersection u1 (Union d d1) ~ '[], LEq st pc) =>  
          Exp env st d vars -> 
          Stm env pc u1 d1 ->
          Stm env pc u1 (Union d d1)

     
