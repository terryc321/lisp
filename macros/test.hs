

twice x = x * 2
add1 x = x + 1


---data Constant = Cint Integer | CTrue | CFalse deriving (Show ,Eq)
-- data Variable = Var String deriving (Show ,Eq)

--- define some variables , shorthand
---vx = Var "x"
---vy = Var "y"
---vz = Var "z"

--- data Dir = D Int String deriving Show
--- Var


--- need some stupid input parser to make an s expression tree
---


---- constants
--- integers -> integers
--- variable -> time stamped variable
--- anything else -> anything else
data Stree = SC Integer | SV String deriving (Show,Eq)

data Tsstree = TC Integer | TV String deriving (Show, Eq)







-- data Time_Stamped_Variable = TStamp Variable Integer deriving (Show ,Eq)

-- uppercase for constructors
-- functions need to be lower case
--- s :: Integer -> Variable -> Time_Stamped_Variable
--- s i v = TStamp v i

-- -- --- S naught
--- s0 = (s 0) 
-- tau = s0

-- --- initially tau is s 0
-- tau = s 0

--- tf
--- tf (Var s) t = \t -> (t (Var s))
--- tf () t = \t -> ()
-- --- Integer -> Const












