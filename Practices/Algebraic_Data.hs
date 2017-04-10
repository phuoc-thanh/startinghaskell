-- Algebraic data types
-- CIS 194 Spring 2015, Lecture 03
-- http://www.cis.upenn.edu/~cis194/spring15/lectures/03-ADTs.html

data Thing = Shoe | Ship | SealingWax | Cabbage | King
  deriving Show

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _    = True

-- Store a person's name, age, and favorite Thing.
data Person = Person String Int Thing
  deriving Show

richard :: Person
richard = Person "Richard" 32 Ship

stan :: Person
stan  = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

-- Patterns can be nested.
checkFav (Person n _ Ship) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)    = n ++ ", your favorite thing is lame."
