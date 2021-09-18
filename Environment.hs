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
infixr 5 :-:

data HList :: [(Nat , SType)] -> * where
    Nil :: HList '[] 
    (:-:) :: (SNat n , SeType s) -> HList xs -> HList ('(n , s) ': xs) 

-- Tipos de las variables

type Zero = 'Zero

type One = 'Succ 'Zero
type Two = 'Succ One
type Three = 'Succ Two
type Four = 'Succ Three
type Five = 'Succ Four
type Six = 'Succ Five
type Seven = 'Succ Six
type Eight = 'Succ Seven
type Nine = 'Succ Eight

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

five :: SNat Five
five = SSucc four

six :: SNat Six
six = SSucc five

seven :: SNat Seven
seven = SSucc six

eight :: SNat Eight
eight = SSucc seven

nine :: SNat Nine
nine = SSucc eight

