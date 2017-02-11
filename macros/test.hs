

twice x = x * 2
add1 x = x + 1

-- the-type : the-meaning

-- v in set var : identifier : symbol?
data Variable = Var String deriving (Show ,Eq)
--- define some variables , shorthand
vx = Var "x"
vy = Var "y"
vz = Var "z"


--- w in set TSVar : time stamped variable 
data Time_Stamped_Variable = Tsv Variable Integer deriving (Show , Eq)

--- some constants
data Constant = Ci Integer | Cb Bool deriving (Show , Eq)

--- data Dir = D Int String deriving Show
--- Var


--- uppercase for constructors
--- functions need to be lower case
s :: Integer -> Variable -> Time_Stamped_Variable
s i v = Tsv v i

--- S naught
s0 v = Tsv v 0 

--- initially tau is s 0
tau = (s 0)



tee (Var name) = \t -> (t (Var name))










--- Integer -> Const









