module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

funcTable :: [(UnOp, (Double -> Double))]
funcTable = [(Neg, (0-)), (Sin, (sin)), (Cos, (cos)), (Log, (log))]

opTable :: [(BinOp, Double -> Double -> Double)]
opTable = [(Add, (+)), (Mul, (*)), (Div, (/))]


lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key ((k, v) : table) 
   |key == k  = v
   |otherwise = lookUp key table

eval :: Exp -> Env -> Double
eval (Val x) table = x
eval (Id x) table  = x'
   where
      x' = lookUp x table
eval (UnApp func exp) table = (lookUp func funcTable) x
   where
      x = eval exp table 
eval (BinApp op exp1 exp2) table = (lookUp op opTable) x y
   where 
      x = eval exp1 table
      y = eval exp2 table

--eval function without tables and using haskell's lookup
eval' :: Exp -> Env -> Double 
eval' (Val x) table = x
eval' (Id x) table  = x'
   where
      x' = fromJust (lookup x table)
eval' (UnApp func exp) table
   |func == Neg = 0 - x
   |func == Sin = sin x
   |func == Cos = cos x
   |func == Log = log x
   where x = eval exp table
eval' (BinApp op exp1 exp2) table
   |op == Add = x + y
   |op == Mul = x * y
   |op == Div = x / y
   where 
      x = eval exp1 table
      y = eval exp2 table

diff :: Exp -> String -> Exp
diff (Id val) x
   |val == x  = Val 1.0
   |otherwise = Val 0.0 
diff (Val v) x = Val 0.0
diff (UnApp func exp) x
   |func == Neg = UnApp Neg (exp')
   |func == Sin = BinApp Mul (UnApp Cos (exp)) (exp')
   |func == Cos = UnApp Neg (BinApp Mul (UnApp Sin (exp)) (exp'))
   |func == Log = BinApp Div (exp') (exp)
   where 
      exp' = diff exp x
diff (BinApp op exp1 exp2) x
   |op == Add = BinApp Add (exp1') (exp2')
   |op == Mul = BinApp Add (BinApp Mul (exp1) (exp2')) (BinApp Mul (exp1') (exp2))
   |op == Div = BinApp Div (BinApp Add (BinApp Mul (exp1') (exp2)) (UnApp Neg (BinApp Mul (exp1) (exp2')))) (BinApp Mul (exp2) (exp2))
   where
      exp1' = diff exp1 x
      exp2' = diff exp2 x
--maclaurin using higher order functions
maclaurin1 :: Exp -> Double -> Int -> Double
maclaurin1 exp x n = foldl (+) 0 (take n (zipWith3 term diffs xpows facts))
   where 
     facts = scanl (*) 1 [1,2 ..] 
     xpows = scanl (*) 1 [x,x ..]
     diffs = map ((flip eval) [("x", 0.0)]) (iterate ((flip diff) "x") exp)
     term x y z = x*y/z

--recursive maclaurin        
maclaurin2 :: Exp -> Double -> Int -> Double
maclaurin2 exp x n = maclaurin2' exp x n 0 1 1
   where 
      maclaurin2' :: Exp -> Double -> Int -> Int -> Double -> Int -> Double
      maclaurin2' exp x n m powx fact
         |m == n    = 0
         |otherwise = f * powx / (fromIntegral fact) + maclaurin2' exp' x n m' powx' fact'
         where 
            powx' = powx * x
            m'    = m + 1
            fact' = fact * m'
            exp'  = diff exp "x"
            f'    = eval exp' [("x", 0.0)]
            f     = eval exp [("x", 0.0)]

showExp :: Exp -> String
showExp (Val x) = show x 
showExp (Id x) = x
showExp (UnApp func exp) 
   |func == Neg = "-" ++ show
   |func == Sin = "sin" ++ show
   |func == Cos = "cos" ++ show
   |func == Log = "log" ++ show 
   where 
      show =  "(" ++ showExp exp ++ ")"
showExp (BinApp op exp1 exp2)
   |op == Add = "(" ++ show1 ++ "+" ++ show2 ++ ")"
   |op == Mul = "(" ++ show1 ++ "*" ++ show2 ++ ")"
   |op == Div = "(" ++ show1 ++ "/" ++ show2 ++ ")"
   where 
      show1 = showExp exp1
      show2 = showExp exp2

---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

----------------------------------------------------------------------
-- EXTENSION: Uncomment and complete these...

-- instance Num Exp where

-- instance Fractional Exp where

-- instance Floating Exp where


-- instance (Eq a, Num a) => Num (Maybe a) where

-- instance (Eq a, Fractional a) => Fractional (Maybe a) where

-- diff2 :: Exp -> String -> Maybe Exp



-- The following makes it much easier to input expressions, e.g. sin x, log(x*x) etc.

x, y :: Exp
x = Id "x"
y = Id "y"
