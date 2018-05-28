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
freeImpl (p:-->q)   = (Not (freeImpl p)) :||: freeImpl q
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
cnfP (p :&&: q)     = cnfP p :&&: cnfP q
cnfP (p :||: q)     = distr (cnfP p) (cnfP q)
cnfP p = p

distr:: Prop -> Prop -> Prop
distr p (q:&&:r)    = distr p q :&&: distr p r
distr (q:&&:r) p    = distr q p :&&: distr r p
distr p q           = p:||:q 

cnf:: Prop -> Prop
cnf a = cnfP(nnf(freeImpl(a)))

--Algoritmo de verificação de tautologia:

positives:: Prop -> [String]
positives (V a) = [a]
positives (Not(V a)) = []
positives (p:||:q) = (positives p) ++ (positives q)
positives _ = error "NONCnf"

negatives:: Prop -> [String]
negatives (V _) = []
negatives (Not(V a)) = [a]
negatives (p:||:q) = (negatives p) ++ (negatives q)
negatives _ = error "NONCnf"

taut:: Prop -> Bool
taut (p:&&:q) = taut p && taut q
taut p = not( null( intersect(positives p) (negatives p)))

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect as bs = let ns = [ a | a <- as, elem a bs] in [ b | b <- bs, elem b ns]

--Funções de teste
ex = ((V "P") :||: (Not(V "P"))) --Deve retornar taut = true, (P | (~P))

ex2 = ((Not(V "P")) :&&: (V "Q")) :--> ((V "P") :&&: ((V "R") :--> (V "Q"))) --Deve retornar taut = false, (((~P) & Q) --> (P & (R --> Q)))

--Funções do trabalho

-- vr = Vinho está ruim
-- av = Alírio toma vinho
-- at = Alírio fica triste
-- ac = Alírio vai pra casa
-- ar = Alírio fica com ressaca
-- ae = Alírio vai ao encontro romântico com Virgínia

hip1 = (((V "av") :&&: (V "vr")) :--> (V "ar"))

hip2 = ((V "fr") :--> ((V "av") :&&: (V "vr")))

hip3 = (V "ae") :||: ((V "at") :&&: (V "ac"))



afirm1 = (((V "av") :&&: (V "vr")) :--> (Not(V "ae")))

afirm2 = (((V "ar") :&&: (V "ac")) :--> (Not(Not(V "ae"))))

afirm3 = ((V "vr") :--> (Not(V "av") :||: Not(V "ar")))

afirm4 = (((V "vr") :||: (V "ar")) :--> (V "at"))

afirm5 = (((V "av") :||: (V "ac")) :--> (Not(V "at") :--> (V "vr")))



prova1 = ((((V "av") :&&: (V "vr")) :--> (V "ar")) :&&: (((V "fr") :--> ((V "av") :&&: (V "vr"))) :&&: ((((V "ae") :||: (V "at")) :&&: (V "ac")) :--> (((V "av") :&&: (V "vr")) :--> Not(V "ae")))))

prova2 = ((((V "av") :&&: (V "vr")) :--> (V "ar")) :&&: (((V "fr") :--> ((V "av") :&&: (V "vr"))) :&&: ((((V "ae") :||: (V "at")) :&&: (V "ac")) :--> (((V "ar") :&&:(V "ac")) :--> (Not(V "ae"))))))

prova3 = ((((V "av") :&&: (V "vr")) :--> (V "ar")) :&&: ((V "fr") :--> ((V "av") :&&: (V "vr"))) :&&: (V "ae") :||: ((V "at") :&&: (V "ac"))) :--> ((V "vr") :--> (Not(V "av") :||: Not(V "ar")))

prova4 = ((((V "av") :&&: (V "vr")) :--> (V "ar")) :&&: ((V "fr") :--> ((V "av") :&&: (V "vr"))) :&&: (V "ae") :||: ((V "at") :&&: (V "ac"))) :--> (((V "vr") :||: (V "ar")) :--> (V "at"))

prova5 = ((((V "av") :&&: (V "vr")) :--> (V "ar")) :&&: ((V "fr") :--> ((V "av") :&&: (V "vr"))) :&&: (V "ae") :||: ((V "at") :&&: (V "ac"))) :--> (((V "av") :||: (V "ac")) :--> (Not(V "at") :--> (V "vr")))






