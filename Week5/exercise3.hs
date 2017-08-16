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
class Expr a where
    lit :: a -> a
    lit x = x
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr Integer where
    mul e1 e2 = e1 * e2
    add e1 e2 = e1 + e2

