module T1 where

data Logic = 
             V String |
            Not Logic |
     Logic :&&: Logic |
     Logic :||: Logic |
     Logic :--> Logic |
     Logic :<->: Logic

type Prop = Logic

--Algoritmo de impressão:

instance Show Logic where
    show (V a)      = a
    show (Not p)    = "(~" ++ show p ++ ")"
    show (p:&&:q)   = "("  ++ show p ++ " & "   ++ show q ++ ")"
    show (p:||:q)   = "("  ++ show p ++ " | "   ++ show q ++ ")"
    show (p:-->q)   = "("  ++ show p ++ " --> " ++ show q ++ ")"
    show (p:<->:q)  = "("  ++ show p ++ " <-> " ++ show q ++ ")";

sugar:: Prop -> String
sugar a = show a

--Algoritmo da forma normal conjuntiva:

freeImpl:: Prop -> Prop
freeImpl (V a)      = V a
freeImpl (Not a)    = Not a
freeImpl (p:-->q)   = freeImpl (Not p :||: q)
freeImpl (p:<->:q)  = freeImpl(p :--> q) :&&: freeImpl (q :--> p)
freeImpl (p:&&:q)   = freeImpl p :&&: freeImpl q
freeImpl (p:||:q)   = freeImpl p :||: freeImpl q

nnf:: Prop -> Prop
nnf (V a)           = V a
nnf (Not (V a))     = Not(V a)
nnf (Not (Not p))   = nnf p
nnf (Not (p:&&:q))  = nnf (Not p :||: Not q)
nnf (Not (p:||:q))  = nnf (Not p :&&: Not q)
nnf (p:&&:q)        = nnf p :&&: nnf q
nnf (p:||:q)        = nnf p :||: nnf q

cnfP:: Prop -> Prop
cnfP (p :&&: q) = cnfP p :&&: cnfP q
cnfP (p :||: q) = distr (cnfP p) (cnfP q)
cnfP p = p

distr:: Prop -> Prop -> Prop
distr p (q:&&:r) = distr p q :&&: distr p r
distr (q:&&:r) p = distr q p :&&: distr r p
distr p q        = p:||:q 

cnf:: Prop -> String
cnf a = sugar(cnfP(nnf(freeImpl(a))))

--Save Point
--Algoritmo de verificação de tautologia:

















