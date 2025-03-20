 
------------------------------------------------------------------- 
-- HASKELL BASIC LIST AND STRING PROCESSING FUNCTIONS           
                                         
------------------------------------------------------------------- 

module Basic where


-- COMPLETE the code for  mem 

{--
A type must be of type Equality to check comparison with [a], produce output bool if in list or not
--}
mem :: Eq a => a -> [a] -> Bool
mem e [] = False
--mem e (x:xs) = if e == x then True else mem e xs
mem e (x:xs) | e == x = True
            | otherwise = mem e xs
-- guarded version/cleaner version



-- COMPLETE  the code for  lookUp
{--
Given a list xs :: [(a,b)] and x :: a, the function lookUp returns an element y :: b such that (x, y) is in the list xs. 
Use the function error that we used in the first worksheets. The exact string that you put in the message is not important.
Used in memory state so important
--}


lookUp :: Eq a => [(a,b)] -> a -> b
lookUp [] e = error "No element exist in memory state"
lookUp ((x, xx):ys) e | x == e = xx
                | otherwise = lookUp ys e




-- COMPLETE  the code for  update
{--
Given a list xs :: [(a,b)] and x ::a and y :: b , the function update does the following:

     if (x, y') is in xs then y is the new value associated to x
    otherwise -- there are no pairs in xs with first component x -- add the pair (x, y) to the list xs

    e = elem , r = replace

    Will use matching patterns and recursion, to construct new list since original is immutable, right associativity so (x,xx):((x,xx):[])
    Base case is [], so if no match found, we cons (e,r) with empty list, saying we add this to the series of tuples before concating with a list

--}


update :: Eq a => [(a, b)] -> a -> b -> [(a, b)]

update [] e r = (e,r):[]

update ((x, xx):ys) e r | x == e = (x, r): ys
                      | otherwise = (x, xx): update ys e r

