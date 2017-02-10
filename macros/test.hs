

twice x = x * 2
add1 x = x + 1

-- the-type : the-meaning

-- v in set var : identifier : symbol?
data Var = V String deriving Show

--- w in set TSVar : time stamped variable 
data TSVar = TSV Var Integer deriving Show

---
data Const = Ci Integer | Cb Bool deriving Show

--- data Dir = D Int String deriving Show
--- Var

--- uppercase for constructors
--- functions need to be lower case
s :: Integer -> Var -> TSVar
s i v = TSV v i





--- Integer -> Const









