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

cnf:: Prop -> Prop
cnf a = cnfP(nnf(freeImpl(a)))

--Algoritmo de verificação de tautologia:

positives:: Prop -> [String]
positives (V a) = [a]
positives (Not(V a)) = []
positives (p:||:q) = (positives p) ++ (positives q)

negatives:: Prop -> [String]
negatives (V _) = []
negatives (Not(V a)) = [a]
negatives (p:||:q) = (negatives p) ++ (negatives q)

taut:: Prop -> Bool
taut (p:&&:q) = taut p && taut q
taut p = not( null( intersect(positives p) (negatives p)))

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect as bs = let ns = [ a | a <- as, elem a bs] in [ b | b <- bs, elem b ns]

--Save Point

ex = ((V "P") :||: (Not(V "P"))) --Deve retornar taut = true, (P | (~P))

ex2 = ((Not(V "P")) :&&: (V "Q")) :--> ((V "P") :&&: ((V "R") :--> (V "Q"))) --Deve retornar taut = false, (((~P) & Q) --> (P & (R --> Q)))


