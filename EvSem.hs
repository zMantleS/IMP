--------------------------------------------------------------------- 
-- SML EVALUATION SEMANTICS FOR IMP                                   
-- Roy Crole and Paula Severi 2024                                            
--------------------------------------------------------------------- 

module EvSem

where

import AST
import Basic


----------------------------------------------------------------
--  Code the function evalint for evaluating Integer Expressions
----------------------------------------------------------------
 

evalint :: IntExp -> State -> Int 
evalint (Int integer) s = integer
evalint (Var var) s =  lookUp s var
evalint (IopExp(iop, iexp1, iexp2)) s | iop == Plus = evalint iexp1 s + evalint iexp2 s
                                    | iop == Minus = evalint iexp1 s - evalint iexp2 s
                                    | iop == Times = evalint iexp1 s * evalint iexp2 s
 
 ---------------------------------------------------------------------
-- Code the function evalbool for evaluating Boolean Expressions
-----------------------------------------------------------------------


evalbool :: BoolExp -> State -> Bool
evalbool (Bool bool) s = bool
evalbool (BopExp(bop, iexp1, iexp2)) s | bop == Le = evalint iexp1 s < evalint iexp2 s
                                    | bop == Gr = evalint iexp1 s > evalint iexp2 s
                                    | bop == LeEq = evalint iexp1 s <= evalint iexp2 s
                                    | bop == GrEq = evalint iexp1 s >= evalint iexp2 s

                                                                                    
--------------------------------------------------------------------------
-- Code the function evalcom for evaluating Commands
---------------------------------------------------------------------------

evalcom :: Com -> State -> State 
evalcom (Ass(var, iexp)) s = update s var (evalint iexp s)
evalcom (If(bexp, com1, com2)) s | evalbool bexp s = evalcom com1 s
                                | otherwise = evalcom com2 s
evalcom (While(bexp1, com1)) s | evalbool bexp1 s = evalcom (While(bexp1, com1)) (evalcom com1 s) 
                            | otherwise = s
evalcom (Seq(com1, com2)) s = let s2 = evalcom com1 s in evalcom com2 s2