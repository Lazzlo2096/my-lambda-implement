{--
"a = λ x . x"
"b = λ x . x x"
"a b"
"b a"
--}

--data MyThings = Lambda String MyThings | Apply MyThings MyThings | Term String
data MyThings = 
      Lambda MyThings MyThings 
    | Apply MyThings MyThings 
    | Term String
        deriving (Eq, Show)

--idComb' = "λ x . x"
idComb = Lambda (Term "x") (Term "x")

--selfApl' = "λ x . x x"
selfApl = Lambda (Term "x") (Apply (Term "x") (Term "x"))

apl = Lambda (Term "y") (Lambda (Term "x") (Apply (Term "y") (Term "x")))

lol = Apply idComb selfApl 
lol2 = Apply (Apply apl idComb) selfApl 

-- MyThings -> 
redApply (Apply t1 t2) = aplToLambda t1 t2

aplToLambda (Lambda t1 t2) t3 = replaceInWith t1 t2 t3

replaceInWith t1 t2@(Lambda t2t1 t2t2) t3 = error "lol"
replaceInWith t1 t2@(Apply t2t1 t2t2)  t3
    | (t1 == t2t1) && (t1 == t2t2) = Apply t3 t3  -- а если одновременнно?? -- error "selfAply"
    | t1 == t2t1 = Apply t3 t2t2
    | t1 == t2t2 = Apply t2t1 t3
    | otherwise = error "ooo"

replaceInWith t1 t2@(Term nameT2)      t3 = if t1 == t2 then t3 else error "lal"

redApply_until_end a@(Apply _ _) = redApply_until_end (redApply a)
redApply_until_end a = a

-- Y = "λ g . "
-- I = "λ x . "
-- K = "λ x . λ y . x"
-- S = " "
-- LOL1 = "λ x . λ y . x y"


main = putStrLn $ show $ redApply_until_end lol
