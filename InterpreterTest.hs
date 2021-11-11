{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}  
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- Para que las clases puedan tener más de un parámetro
{-# LANGUAGE FlexibleInstances #-} -- Para usar variables en las instancias de clases
{-# LANGUAGE FlexibleContexts #-} -- Lo Agregué
{-# LANGUAGE UndecidableInstances #-} -- Lo Agregué
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}


module InterpreterTest where


import           DR
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple             as P
import           Control.Monad                  ( liftM
                                                , ap
                                                )
												
import Control.Monad.Reader

-- Entornos
-- Asociación de nombres de variables y sus respectivos valores
-- TODO Borrar el primer Int, que hace referencia a las variables.
type Enviroment = M.Map Int Int


-- Entorno nulo
initMemory :: Enviroment
initMemory = M.empty


-- Mónada estado
newtype State a = State { runState :: Enviroment -> P.Pair a Enviroment }


instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')
  
  
-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap
  
  
instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert
  
  
  
toInt :: SNat n -> Int
toInt SZero = 0
toInt (SSucc x) = 1 + (toInt x)

--------------------------------------- 
-- Expresiones
---------------------------------------
evalExp ::  Exp a b c d -> State Int
evalExp (IntLit i)  = return i
-- En principio usamos los enteros como booleanos, para no cambiar sistema de tipos
evalExp (BoolLit i)  = return (boolToInt i)
evalExp (Var x)    =  lookfor (toInt x)
evalExp (Declassify e l)    = evalExp e -- TODO PROBRAR
evalExp (Ope Plus e1 e2)  =  evalBIntegerOp e1 e2 (+)
evalExp (Ope Minus e1 e2)  =  evalBIntegerOp e1 e2 (-)
evalExp (Ope Mult e1 e2)  =  evalBIntegerOp e1 e2 (*)
evalExp (Ope Div e1 e2)   = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case v2 of
    _ -> return (div v1 v2)
evalExp (Ope Exp e1 e2) = evalBIntegerOp e1 e2 (^)
evalExp (Ope Mod e1 e2) = evalBIntegerOp e1 e2 (mod)
evalExp (Ope And e1 e2) = evalBIntegerOp e1 e2 (andOp) 
evalExp (Ope Or e1 e2) = evalBIntegerOp e1 e2 (orOp) 
evalExp (Ope Gt e1 e2) = evalBBooleanOp e1 e2 (>)
evalExp (Ope GtE e1 e2) = evalBBooleanOp e1 e2 (>=) 
evalExp (Ope Lt e1 e2) = evalBBooleanOp e1 e2 (<) 
evalExp (Ope LtE e1 e2) = evalBBooleanOp e1 e2 (<=) 
evalExp (Ope Eq e1 e2) = evalBBooleanOp e1 e2 (==) 
evalExp (Ope NotEq e1 e2) = evalBBooleanOp e1 e2 (/=) 


evalBBooleanOp e1 e2 f = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (boolToInt (f v1 v2))

evalBIntegerOp e1 e2 f = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (f v1 v2)
  
boolToInt False = 0
boolToInt True = 1

-- 0 false y distinto de 0 True
andOp 0 x = 0
andOp x 0 = 0
andOp x y = 1


orOp 0 0 = 0
orOp x y = 1



---------------------------------------
-- SENTENCIAS
---------------------------------------
-- Evalua un programa en el estado nulo
evalStm :: Stm a b c d -> Enviroment
evalStm p = snd (runState (eval p) initMemory)


-- Evalua un programa en un estado dado.
evalStmWithEnviroment :: Stm a b c d -> Enviroment -> Enviroment
evalStmWithEnviroment p env = snd (runState (eval p) env)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip

eval :: Stm a b c d -> State ()
eval Skip = return ()
eval (Ass x e) = do
  v <- evalExp e
  update (toInt x) v
--  eval Skip
eval (Seq c0  c1) =  (eval c0) >> (eval c1) -- TODO Revisar
eval (If b c0 c1) = do
  vb <- evalExp b
  if vb == 1 then eval c0 else eval c1 
eval w@(While b c) = do
  vb <- evalExp b
  if vb==1 then eval (Seq c w) else return () --eval Skip

  




