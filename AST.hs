  
--------------------------------------------------------------------
-- HASKELL ABSTRACT SYNTAX TREES FOR IMP                  
                                             
--------------------------------------------------------------------

module AST where 

type V = String
type B = Bool
type Z = Int
type FileName = String

data Bop =   Le | Gr | LeEq | GrEq
             deriving (Eq,Show)

data Iop = Plus | Minus | Times 
             deriving (Eq,Show)
             


-- COMPLETE the three data types below 

data Com   = Ass(V, IntExp) | If(BoolExp, Com, Com) | While(BoolExp, Com) | Seq(Com, Com) 
                    deriving (Eq,Show)

data BoolExp   =  Bool B | BopExp(Bop, IntExp, IntExp)
                    deriving (Eq,Show)
             
data IntExp   =  Int Z | Var V | IopExp(Iop, IntExp, IntExp)
                    deriving (Eq,Show)








-- COMPLETE the definition of the type State
type State = [(V, Z)]


--- Once completed, uncomment what is below: 



data Instruction = Run FileName | Eval Prog | Helpp | Help Int | Quit 
                   deriving (Eq,Show)

type Prog  = (Code,State)  


data Code  = E IntExp | C Com
             deriving (Eq,Show)







