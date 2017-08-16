import Parser

data ExprT  = Lit Integer
            | Add ExprT ExprT
            | Mul ExprT ExprT
  deriving (Show, Eq)


--Exercise 1
--Write Version 1 of the calculator: an evaluator for ExprT , with the signature
eval :: Maybe ExprT -> Maybe Integer
eval Nothing = Nothing
eval (Just e) = Just (eval' e)

eval' :: ExprT -> Integer
eval' (Lit x) = x
eval' (Add e1 e2) = (eval' e1) + (eval' e2)
eval' (Mul e1 e2) = (eval' e1) * (eval' e2)

-- Exercise 2
-- The UI department has internalized the focus group data and is
-- ready to synergize with you. They have developed the front-facing
-- user-interface: a parser that handles the textual representation of the
-- selected language. They have sent you the module Parser.hs, which exports
-- parseExp , a parser for arithmetic expressions. If you pass the constructors of
-- ExprT to it as arguments, it will convert Strings representing arithmetic 
-- expressions into values of type ExprT . For example:

-- Calc> parseExp Lit Add Mul "(2+3) * 4"
-- Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
-- Calc> parseExp Lit Add Mul "2+3 * 4"
-- Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
-- Calc> parseExp Lit Add Mul "2+3 * "
-- Nothing

-- Leverage the assets of the UI team to implement the value-added function
-- evalStr :: String -> Maybe Integer
-- which evaluates arithmetic expressions given as a String , producing Nothing
-- for inputs which are not well-formed expressions, and Just n for well-formed 
-- inputs that evaluate to n

evalStr :: String -> Maybe Integer
evalStr = eval . parseExp Lit Add Mul

-- Exercise 3
-- Good news! Early customer feedback indicates that people really
-- do love the interface! Unfortunately, there seems to be some disagree-
-- ment over exactly how the calculator should go about its calculating
-- business. The problem the software department (i.e. you) has is that
-- while ExprT is nice, it is also rather inflexible, which makes catering
-- to diverse demographics a bit clumsy. You decide to abstract away
-- the properties of ExprT with a type class.  Create a type class called
-- Expr with three methods called lit , add , and mul which parallel the 
-- constructors of ExprT. Make an instance of Expr for the ExprT
-- type, in such a way that
-- 
--     homework 5  3
--     mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
--     == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
-- 
-- Think carefully about what types lit , add , and mul should have. It
-- may be helpful to consider the types of the ExprT constructors, which
-- you can find out by typing (for example)
-- 
--     Calc> :t Lit
-- 
-- at the ghci prompt.  Remark.  Take a look at the type of the foregoing example expression:
-- 
--     Calc> :t mul (add (lit 2) (lit 3)) (lit 4)
--     Expr a => a
-- 
-- What does this mean? The expression mul (add (lit 2) (lit 3)) (lit 4) has
-- any type which is an instance of the Expr type class. So writing it
-- by itself is ambiguous: GHC doesn’t know what concrete type you
-- want to use, so it doesn’t know which implementations of mul , add , and lit
-- to pick.
-- One way to resolve the ambiguity is by giving an explicit type
-- signature, as in the above example. Another way is by using such an
-- expression as part of some larger expression so that the context in
-- which it is used determines the type. For example, we may write a
-- function reify as follows:
-- 
--     reify :: ExprT -> ExprT
--     reify = id
-- 
-- To the untrained eye it may look like reify does no actual work!
-- But its real purpose is to constrain the type of its argument to
-- ExprT .  Now we can write things like 
-- 
--     reify $ mul (add (lit 2) (lit 3)) (lit 4)
-- 
-- at the ghci prompt.
class Expr expression where
    lit :: Integer -> expression
    mul :: expression -> expression -> expression
    add :: expression -> expression -> expression

instance Expr ExprT where
    lit i = Lit i
    mul e1 e2 = Mul e1 e2
    add e1 e2 = Add e1 e2

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
-- The marketing department has gotten wind of just how flexible
-- the calculator project is and has promised custom calculators to some
-- big clients. As you noticed after the initial roll-out, everyone loves the
-- interface, but everyone seems to have their own opinion on what the
-- semantics should be. Remember when we wrote ExprT and thought
-- that addition and multiplication of integers was pretty cut and dried?
-- Well, it turns out that some big clients want customized calculators
-- with behaviors that they have decided are right for them.
-- The point of our Expr type class is that we can now write down
-- arithmetic expressions once and have them interpreted in various
-- ways just by using them at various types.
-- 
-- Make instances of Expr for each of the following types:
-- 
-- • Integer — works like the original calculator
-- • Bool — every literal value less than or equal to 0 is in- terpreted as
--     False , and all positive Integer s are interpreted as True
--     ; “addition” is logical or, “multiplication” is logical and
-- • MinMax — “addition” is taken to be the max function, while
--     “multiplication” is the min function
-- • Mod7 — all values should be in the ranage 0 . . . 6, and
--     all arithmetic is done modulo 7 ; for example, 5 + 3 = 1.
-- 
-- The last two variants work with Integer s internally, but in order
-- to provide different instances, we wrap those Integer s in
-- newtype wrappers. These are used just like the data constructors we’ve seen
-- before.  
-- 
--     newtype MinMax  = MinMax Integer deriving (Eq, Show)
--     newtype Mod7    = Mod7 Integer deriving (Eq, Show)
-- 
-- Once done, the following code should demonstrate our family of
-- calculators:
-- 
-- testExp :: Expr a => Maybe a
-- testExp = parseExp lit add mul "(3 * -4) + 5"
-- testInteger  = testExp :: Maybe Integer
-- testBool     = testExp :: Maybe Bool
-- testMM       = testExp :: Maybe MinMax
-- testSat      = testExp :: Maybe Mod7
-- Try printing out each of those tests in
-- 
-- ghci to see if things are working.

instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

instance Expr Bool where
    lit = (>0)
    mul = (&&)
    add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
instance Expr MinMax where
    lit i = MinMax i
    mul = min
    add = max

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit i = Mod7 (i `mod` 7)
    mul (Mod7 i) (Mod7 j) = lit (i * j)
    add (Mod7 i) (Mod7 j) = lit (i + j)


