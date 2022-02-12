{-# LANGUAGE TypeFamilies #-}



module Source.Interpreter where


import           Source.DelimitedRelease
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
import Control.Monad.State 

-- Environment
-- Map between variables and assigned values
type Enviroment = M.Map Int Int


-- Empty environment
initMemory :: Enviroment
initMemory = M.empty


type Result a = StateT Enviroment Maybe a
  
  
  
toInt :: SNat n -> Int
toInt SZero = 0
toInt (SSucc x) = 1 + (toInt x)


update v i = StateT (\s -> Just ((), update' v i s)) where update' = M.insert
lookfor x = do env <- get
               case  M.lookup x env of
				     Nothing -> throw
				     Just y -> return y
				
throw = StateT (\s -> Nothing)
			   


--------------------------------------- 
-- Expresiones
---------------------------------------
evalExp ::  Exp a b c d -> Result Int
evalExp (IntLit i)  = return i
evalExp (BoolLit i)  = return (boolToInt i)

evalExp (Var x)    =   lookfor (toInt x)
evalExp (Declassify e l)    = evalExp e 
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
-- Commands
---------------------------------------

-- Evaluates a program given an empty state.
evalStm :: Stm a b c d -> Enviroment
evalStm p =  let (x, y) = fromJust $ (runStateT (eval p) initMemory) 
             in y


-- Evaluates a program given a state.
evalStmWithEnviroment :: Stm a b c d -> Enviroment -> Enviroment
evalStmWithEnviroment p env = let (x, y) = fromJust $ (runStateT (eval p) env)
							  in y

-- Evaluates multiple steps of a command, ultil it reaches Skip
eval :: Stm a b c d -> Result ()
eval Skip = return ()
eval (Ass x e) = do
  v <- evalExp e
  update (toInt x) v
  eval Skip
eval (Seq c0  c1) =  (eval c0) >> (eval c1) 
eval (If b c0 c1) = do
  vb <- evalExp b
  if vb == 1 then eval c0 else eval c1 
eval w@(While b c) = do
  vb <- evalExp b
  if vb==1 then eval (Seq c w) else return () 





