module T1 where

data Logic = 
             V String |
            Not Logic |
     Logic :&&: Logic |
     Logic :||: Logic |
     Logic :--> Logic |
     Logic :<->: Logic

type Prop = Logic

instance Show Logic where
    show (V a)             = a
    show (Not p)           = "(~" ++ show p ++ ")"
    show (p:&&:q)  = "("  ++ show p ++ " & "   ++ show q ++ ")"
    show (p:||:q)  = "("  ++ show p ++ " | "   ++ show q ++ ")"
    show (p:-->q)  = "("  ++ show p ++ " --> " ++ show q ++ ")"
    show (p:<->:q) = "("  ++ show p ++ " <-> " ++ show q ++ ")";

exPrint = (V "A"):-->(V "B"):-->(V "A")


freeImpl:: Prop -> Prop
freeImpl (V a)        = (V a)
freeImpl (Not a)      = (Not a)
freeImpl (p:&&:q)     = (freeImpl p:&&:freeImpl q)
freeImpl (p:||:q)     = (freeImpl p:||:freeImpl q)
freeImpl (p:<->:q)    = freeImpl(p :--> q) :&&: freeImpl(q :--> p)
freeImpl (p:-->q)     = freeImpl ((Not q) :||: p);

exFreeImpl = (V "A"):<->:(V "B")

nnf:: Prop -> Prop
nnf (V a) = V a
nnf (Not (V a)) = Not(V a)
nnf (Not (Not p))  = nnf p
nnf (Not (p:&&:q)) = nnf ((Not p) :||: (Not q))
nnf (Not (p:||:q)) = nnf ((Not p) :&&: (Not q))
nnf (p:&&:q) = (nnf p :&&: nnf q)
nnf (p:||:q) = (nnf p :||: nnf q);

exNnf = Not(Not(V "A"))

--SAVE POINT
