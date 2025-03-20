--------------------------------------------------------------------
-- HASKELL PARSING LIBRARY                       
-- Roy Crole and Paula Severi 2024
--------------------------------------------------------------------


module Parse 

where

import Tokens
import AST

infixr 5 `next`
infixl 3 `build`
infixl 0 `alt`

---------------------
-- Type of parsers --
---------------------

-- type of parser output: failing, or success with a result of type a 
data Pout t = FailAs String | Success t
              deriving (Eq,Show) 

-- parser type: a parser takes a list of tokens; we return 
type Parse a  = Tokens -> Pout (a,Tokens)

-------------------
-- Basic parsers --
-------------------

-- Fail on any input (you may not need this) 
failP :: Parse a 
failP toks = FailAs "We always fail"

-- Succeed with the value given (you may not need this) 
succeed :: a -> Parse a 
succeed val toks = Success (val, toks)



key :: IMPword -> Parse IMPword
key kws (Key x : toks) = if kws==x then Success (x,toks) else FailAs ("Found a Key Token .. " ++ x ++ " .. but it does not match the input keyword/symbol")
key kws _ = FailAs "Can't find the keyword/symbol .. "


-- COMPLETE the code for idr 

idr :: Parse IMPword
idr (Id x : toks) = Success(x, toks) 
idr _ = FailAs "Can't find any Idr .."



-- COMPLETE the code for num

num :: Parse IMPword
num (Num x : toks) = Success(x, toks)
num _ = FailAs "Can't find any Num .."


 -------------------------
-- Parsing combinators --
-------------------------


alt :: Parse a -> Parse a -> Parse a
alt ph1 ph2 toks = case ph1 toks of
                     FailAs _ -> ph2 toks
                     Success (r,toks') -> Success (r,toks')


-- COMPLETE the code for  next


next :: Parse a -> Parse b -> Parse (a,b)
next ph1 ph2 toks = case ph1 toks of
                    FailAs _ -> FailAs "next: ph1 fails"
                    Success(r1, toks') ->
                      case ph2 toks' of
                      FailAs _ -> FailAs "next: ph2 fails"
                      Success(r2, toks'') -> Success((r1,r2), toks'')
                      
                
                   

-- Repetition 

many :: Parse a -> Parse [a] 
many ph  = (ph `next` many ph `build` cons) `alt` (\toks -> Success ([], toks))
             where
             cons (x,xs) = x:xs



-- Semantic action. The results from a parser ph are transformed by applying a function f.
build :: Parse a -> (a -> b) -> Parse b 
build ph f toks = case ph toks of
                    FailAs _ -> FailAs "Parser for build fails"  
                    Success (r,toks') -> Success (f r, toks')

makePlus (ie1 , ("+", ie2)) = IopExp (Plus, Int (read ie1), Int (read ie2))

-- reader returns a function which maps an IMPfile to a Haskell term of type a (eg BoolExp, Com, etc).
-- A parser p :: Parse a can be converted to such a function:
-- Note there is an error if the input file is not fully consumed by ph 

reader :: Parse a -> IMPFile -> a
reader ph impf
  = case ph (tokenize impf) of
      Success (result,[]) -> result
      other -> error "parse unsuccessful" 
  

