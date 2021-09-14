{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Environment where

import DR

-- se define en cada programa (A lo mejor se puede definir una type family para que lo defina, para que quede type Env = entorno xs, y xs es una lista de tuplas de enteros, con el nivel de seguridad )

-- Objetivo: quiero crear un tipo de la forma de Env, Env es una lista heterogéna de tipos
-- sus elementos pueden ser distintos tipos, sí tienen el mismo kind (Nat,SType) 
--
-- Necesitamos poder definir una lista a nivel de valores que tenga como tipo una lista heterogénea de tipos 
data HList :: [(Nat , SType)] -> * where
    HNil :: HList '[] 
    HCons :: (SNat n , SeType s) -> HList xs -> HList ('(n , s) ': xs)  

-- Γ : avg-attack.hs
avgAttack = HCons (zero,L) (HCons (one, H) (HCons (two, H) (HCons (three, H) (HCons (four, H) HNil))))
-- Γ : correct-flow.hs
correctFlow = HCons (zero,H) (HCons (one, L) (HCons (two, L) HNil))
-- Γ : wallet.hs
wallet = HCons (zero,H) (HCons (one, L) (HCons (two, L) HNil))
-- Γ : implicit-flow.hs
implicitFlow = HCons (zero,H) (HCons (one, L) HNil)
-- Γ : incorrect-assigment.hs
incorrectAssigment = HCons (zero,L) (HCons (one, H) HNil)
-- Γ : parity-safe.hs
paritySafe = HCons (zero,H) (HCons (one, L) HNil)
-- Γ : safe-update.hs
safeUpdate = HCons (zero,L) (HCons (one, H) (HCons (two, H) HNil))
-- Γ : safe-while.hs
safeWhile = HCons (zero, H) (HCons (one, L) (HCons (two, L) (HCons (three, L) (HCons (four, L) HNil))))
-- Γ : wallet-attack.hs
walletAttack = HCons (zero, H) (HCons (one, L) (HCons (two, L) (HCons (three, L) (HCons (four, L) HNil))))


-- Tipos de las variables

type Zero = 'Zero

type One = 'Succ 'Zero
type Two = 'Succ One
type Three = 'Succ Two
type Four = 'Succ Three

-- Variables

zero  :: SNat Zero
zero = SZero

one  :: SNat One
one = SSucc SZero

two  :: SNat Two
two = SSucc one

three :: SNat Three
three = SSucc two

four :: SNat Four
four = SSucc three

