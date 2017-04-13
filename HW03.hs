module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

extend :: State -> String -> Int -> State
extend st k v k'
    | k' == k = v
    | otherwise = st k'

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

evalE :: State -> Expression -> Int
evalE st (Var v) = st v
evalE _ (Val i) = i
evalE st (Op lEx op rEx) =
    case op of
        Plus   -> x + y
        Minus  -> x - y
        Times  -> x * y
        Divide -> x `div` y
        Gt     -> boolToInt $ x > y
        Ge     -> boolToInt $ x >= y
        Lt     -> boolToInt $ x < y
        Le     -> boolToInt $ x <= y
        Eql    -> boolToInt $ x == y
    where x = evalE st lEx
          y = evalE st rEx


-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign v e)       = DAssign v e
desugar (If e st st')      = DIf e (desugar st) (desugar st')
desugar (Incr v)           = DAssign v (Op (Var v) Plus (Val 1) )
desugar (While e st)       = DWhile e (desugar st)
desugar (For st e st' std) = DSequence (desugar st) (DWhile e d)
    where d = DSequence (desugar std) (desugar st')
desugar (Sequence st st')  = DSequence (desugar st) (desugar st')
desugar Skip               = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign v e) = extend st v (evalE st e)
evalSimple st (DIf e x y)
    | (evalE st e) == 1 = evalSimple st x
    | otherwise = evalSimple st y
evalSimple st w@(DWhile e x)
    | (evalE st e) == 1 = evalSimple (evalSimple st x) w
    | otherwise = st
evalSimple st (DSequence x y) = evalSimple (evalSimple st x) y
evalSimple st DSkip = st

run :: State -> Statement -> State
run st stmt = evalSimple st (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]

-- Thanks to https://gist.github.com/adolfopa/2df36cc66dc7ecd2985e                