module T1 where

data Logic = 
             V String |
            Not Logic |
     Logic :&&: Logic |
     Logic :||: Logic |
     Logic :--> Logic |
     Logic :<->: Logic

type Prop = [Logic]

instance Show Logic where
    show (V a)             = a
    show (Not p)           = "(~" ++ show p ++ ")"
    show (p:&&:q)  = "("  ++ show p ++ " & "   ++ show q ++ ")"
    show (p:||:q)  = "("  ++ show p ++ " | "   ++ show q ++ ")"
    show (p:-->q)  = "("  ++ show p ++ " --> " ++ show q ++ ")"
    show (p:<->:q) = "("  ++ show p ++ " <-> " ++ show q ++ ")";

exProp = ((V "A"):-->(V "B")):-->((V "A"):<->:(V "B"))
--SAVE POINT


