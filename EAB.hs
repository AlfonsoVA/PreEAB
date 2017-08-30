module EAB where
import Data.List
type Nombre = String
data VBool = T | F deriving(Show)


data E = Var Nombre|N Int| True | False | VB Bool|If E E E|Suma E E| Prod E E|Ltn E E|Eqn E E| Conj E E|Disy E E|Neg E|Iz E| Suc E|Pred E| Let Nombre E E deriving(Show)

type Sust = (Nombre, E)

------------------------------------------------------
-- HOLAAAAAAAAAAA
-- AHHH
sustituye :: E -> Sust -> E
sustituye (N n) _ = (N n)
sustituye (VB b) _ = (VB b)
sustituye (Var x) (y,e) = if x == y then e else (Var x)
sustituye (Neg e) l = sustituye e l 
sustituye (Iz e) l = sustituye e l 
sustituye (Suc e) l = sustituye e l
sustituye (Pred e) l = sustituye e l
sustituye (Suma e1 e2) l = (Suma (sustituye e1 l) (sustituye e2 l))
sustituye (Prod e1 e2) l = (Prod (sustituye e1 l) (sustituye e2 l))
sustituye (Ltn e1 e2) l = (Ltn (sustituye e1 l) (sustituye e2 l))
sustituye (Eqn e1 e2) l = (Eqn (sustituye e1 l) (sustituye e2 l))
sustituye (Conj e1 e2) l = (Conj (sustituye e1 l) (sustituye e2 l))
sustituye (Disy e1 e2) l = (Disy (sustituye e1 l) (sustituye e2 l))
sustituye (If e1 e2 e3) l = (If (sustituye e1 l) (sustituye e2 l) (sustituye e3 l))
--sustituye (Let x y z) (n,e) = if (x `elem` ([n]`union` fv y)) then error "Nel" else (Let x y (sustituye z (n,e)))

programafalla = sustituye (Let "x" (Suma (Var "z")(N 1)) (Prod (Var "x")(N 2))) ("x", (N 100))
programafun = sustituye (Let "y" (Suma (Var "z")(N 1)) (Prod (Var "y")(N 2))) ("z", (N 100))


eval :: E -> E
eval (N n) = (N n)
eval (Var n) = (Var n)
eval (VB n) = (VB n)
eval (N n) = (N n)
eval (Suc (N m)) = (N m+1)
eval (Suc n) = (Suc eval n)
eval (Pred (N 0)) = (N 0)
eval (Pred (N m)) = (N m-1)
eval (Neg (VB True)) = (VB False)
eval (Neg (VB False)) = (VB True)
eval (Suma (N x)(N y)) = (N (x+y))
eval (Suma e (N y)) = (Suma (eval e) (N y))
eval (Suma (N x) e) = (Suma (N x) (eval e))
eval (Suma e1 e2) = (Suma (eval e1) (eval e2))
eval (Prod (N x)(N y)) = (N (x*y))
eval (Prod e (N y)) = (Prod (eval e) (N y))
eval (Prod (N x) e) = (Prod (N x) (eval e))
eval (Prod e1 e2) = (Prod (eval e1) (eval e2))

