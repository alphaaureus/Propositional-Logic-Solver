import Data.Char
import Data.List

type Clause = ([Int],[Int])
type Clauses = [Clause]

data PropExpr = Not PropExpr
    | PropExpr :->: PropExpr
    | PropExpr :&: PropExpr
    | PropExpr :|: PropExpr
    | PropExpr :==: PropExpr
    | Variable String
    | Empty

instance Show PropExpr where
    show (Not (Variable v)) = "~" ++ v
    show (Not e) = "~(" ++ show e ++ ")"
    show (lhs :&: rhs) = "(" ++ show lhs ++ " AND " ++ show rhs ++ ")"
    show (lhs :|: rhs) = "(" ++ show lhs ++ " OR " ++ show rhs ++ ")"
    show (lhs :->: rhs) = "(" ++ show lhs ++ " -> " ++ show rhs ++ ")"
    show (lhs :==: rhs) = "(" ++ show lhs ++ " == " ++ show rhs ++ ")"
    show (Variable var) = var

data Token = TokVar String
           | TokNot        | TokAnd     | TokOr
           | TokImplies    | TokFollows | TokEquiv
           | TokUnequal    | TokLpar    | TokRpar
           | TokEOF
  deriving (Show, Eq)

type Tokens = [Token]

----- Variables -----

variables :: String -> [(Int, String)]
variables x = formatVars 0 (reducedAllVar x) -- formats all the variables found in reducedVar

formatVars :: Int -> [String] -> [(Int, String)]
formatVars _ [] = []
formatVars num (x:xs) = (num, x) : formatVars (num+1) xs -- makes tuples for all variables with an increasing INT

reducedAllVar :: String -> [String]
reducedAllVar x = filterRedundants "" (sort (allVar x [])) -- filters the redundant variables of the sorted list of all variable occurences
    where
        filterRedundants word [] = []
        filterRedundants word (x:xs)
            | word == x = filterRedundants word xs -- if the word has already been seen, skip it
            | otherwise = x : filterRedundants x xs

allVar :: String -> [String] -> [String]
allVar [] vars = vars
allVar (x:xs) vars
    | fEntails = allVar sEntails vars -- checks if the word is entails and skips entails
    | fAnd = allVar sAnd vars -- checks if the word is and and skips and
    | fOr = allVar sOr vars -- checks if the word is or and skips or
    | isAlpha x = allVar sVar (vars ++ [fVar]) -- checks if there is a letter and adds the variable to the list
    | otherwise = allVar xs vars -- skips punctuation and whitespace
        where
            (fEntails, sEntails) = matched (x:xs) "ENTAILS"
            (fAnd, sAnd) = matched (x:xs) "AND"
            (fOr, sOr) = matched (x:xs) "OR"
            (fVar, sVar) = getVar (x:xs) ""


matched :: String -> String -> (Bool, String)
matched [] _ = (False, "")
matched x [] = (True, x)
matched (x:xs) (y:ys) -- compares two strings letter by letter, in y we input reference strings for the keywords
    | fromEnum x == fromEnum y || fromEnum x == (fromEnum y + 32) = matched xs ys -- allows matches of upper and lowercase
    | otherwise = (False, xs)

getVar :: String -> String -> (String, String)
getVar [] acc = (acc, [])
getVar (x:xs) acc
    | isAlpha x = getVar xs (acc ++ [x]) -- gets the variable name while there are still letters
    | otherwise = (acc, x:xs)


----- Splitting ----- 

splitInput :: String -> ([String],[String])
splitInput x
    | first == [""] = ([], second) -- Checks if a list only contains the empy string and replaces it by []
    | second == [""] = (first, []) -- Checks if a list only contains the empy string and replaces it by []
    | otherwise = (first, second)
        where
            (first, second) = splitted x "" -- No more removing whitespaces because it is not needed anymore

-- Function skips previous unecessary formatting steps and skips to splitting at colons and entails
splitted :: String -> String -> ([String], [String])
splitted [] acc = (splitColons acc,[])
splitted (x:xs) acc
    | fEntails = (splitColons acc , splitColons sEntails) -- return a tuple of the part before entails and the part after
    | otherwise =  splitted xs (acc ++ [x])
        where
            (fEntails, sEntails) = matched (x:xs) "ENTAILS"

splitColons :: String -> [String]
splitColons [] = []
splitColons (x:xs) = fPortion : splitColons sPortion -- gets the String portion by portion
    where
        (fPortion, sPortion) = portionSplit (x:xs) "" -- finds a portion and returns the portion and then the remaining string

portionSplit :: String -> String -> (String, String)
portionSplit [] acc = (acc, "")
portionSplit (x:xs) acc
    | x == ';' = (acc, xs) -- returns the tuple when the ; is found
    | otherwise = portionSplit xs (acc ++ [x]) -- accumulates till the ;

matchedArrow :: String -> String -> (Bool, String)
matchedArrow [] _ = (False, "")
matchedArrow x [] = (True, x)
matchedArrow (x:xs) (y:ys) -- compares two strings letter by letter, in y we input reference strings for the arrows
    | x == y = matchedArrow xs ys
    | otherwise = (False, xs)


----- Lexer -----

lexer :: String -> Tokens
lexer []                       = [TokEOF]
lexer (x:xs)
    | x == '~' = TokNot : lexer xs
    | fAnd = TokAnd : lexer sAnd
    | fOr = TokOr : lexer sOr
    | x == '-' && head xs == '>' = TokImplies : lexer (tail xs)
    | x == '<' && head xs == '-' = TokFollows : lexer (tail xs)
    | x == '=' && head xs == '=' = TokEquiv : lexer (tail xs)
    | x == '/' && head xs == '=' = TokUnequal : lexer (tail xs)
    | x == '(' = TokLpar : lexer xs
    | x == ')' = TokRpar : lexer xs
    | isAlpha x = TokVar fVar : lexer sVar
    | otherwise = lexer xs
        where
            (fAnd, sAnd) = matched (x:xs) "AND"
            (fOr, sOr) = matched (x:xs) "OR"
            (fVar, sVar) = getVar (x:xs) ""


----- Parser -----


{- parser for the grammar:
    Expr -> Eq
    Eq -> Arr Eq'
    Eq' -> == Arr Eq' | /= Arr Eq' | <empty string>
    Arr -> OR Arr'
    Arr' -> <- OR Arr' | -> OR Arr' | <empty string>
    OR -> AND OR'
    OR' -> or AND OR' | <empty string>
    AND -> Not AND'
    AND' -> and Not AND' | <empty string>
    Not ->  ~ Not | <variable> 
-}

parser :: String -> PropExpr
parser str = fst (parseExpr (Empty, lexer str)) -- Checks if a list only contains the empy string and replaces it by []

-- Expr -> Eq
parseExpr :: (PropExpr,Tokens) -> (PropExpr,Tokens)
parseExpr = parseEq

--  Eq -> Arr Eq'
parseEq :: (PropExpr,Tokens) -> (PropExpr,Tokens)
parseEq = parseEq'.parseArr

-- Eq' -> == Arr Eq' | /= Arr Eq' | <empty string>
parseEq' :: (PropExpr,Tokens) -> (PropExpr,Tokens)
parseEq' (acc, tok:toks)
    | tok == TokEquiv = parseEq' (acc :==: prop, rest) -- Includes ==
    | tok == TokUnequal = parseEq' (Not (acc :==: prop), rest) -- Includes ==
        where
            (prop, rest) = parseArr (Empty,toks)
parseEq' (acc,toks)      = (acc, toks)

-- Arr -> OR Arr'
parseArr :: (PropExpr,Tokens) -> (PropExpr,Tokens)
parseArr = parseArr'.parseOR

-- Arr' -> <- OR Arr' | -> OR Arr' | <empty string>
parseArr' :: (PropExpr,Tokens) -> (PropExpr,Tokens)
parseArr' (acc, tok:toks)
    | tok == TokImplies = parseArr' (acc :->: prop, rest) -- Includes ->
    | tok == TokFollows = parseArr' (prop :->: acc, rest) -- Flips ->
        where
            (prop, rest) = parseOR (Empty,toks)
parseArr' (acc,toks)      = (acc, toks)

-- OR -> AND OR'
parseOR :: (PropExpr,Tokens) -> (PropExpr,Tokens)
parseOR = parseOR'.parseAND

-- OR' -> or AND OR' | <empty string>
parseOR' :: (PropExpr,Tokens) -> (PropExpr,Tokens)
parseOR' (acc, TokOr:toks) = parseOR' (acc :|: prop, rest) -- Includes OR
    where
        (prop, rest) = parseAND (Empty, toks)
parseOR' (acc,toks)      = (acc, toks)

-- AND -> Not AND'
parseAND :: (PropExpr,Tokens) -> (PropExpr,Tokens)
parseAND = parseAND'.parseNot

-- AND' -> and Not AND' | <empty string>
parseAND' :: (PropExpr,Tokens) -> (PropExpr,Tokens)
parseAND' (acc, TokAnd:toks) = parseAND' (acc :&: prop, rest)
    where
        (prop, rest) = parseNot (Empty, toks)
parseAND' (acc,toks)      = (acc, toks)

-- Not ->  ~ Not | <variable> 
parseNot :: (PropExpr,Tokens) -> (PropExpr,Tokens)
parseNot (acc,[])      =  (Empty, [])
parseNot (acc,tok:toks)
    | isTokVar tok =  (Variable (stringTok tok), toks) -- If it is a variable
    | tok == TokLpar && head restE == TokRpar = (propE, tail restE)  -- If there are ()
    | tok == TokNot = (Not propN, restN) -- Negation
    | otherwise          =   (Empty, [])
        where
            (propE, restE) = parseExpr (Empty, toks)
            (propN, restN) = parseNot (Empty, toks)

-- Checking if it is a variable
isTokVar :: Token -> Bool
isTokVar (TokVar _) = True
isTokVar _ = False

-- Getting the name of the variable
stringTok :: Token -> String
stringTok (TokVar varName) = varName
stringTok _ = error "Not a TokVar" -- If misused then there is an error

----- CNF -----

getInput :: String -> (PropExpr, PropExpr)
getInput str = (mergeAnd fSplit, Not (mergeAnd sSplit)) -- Entailment is negated
    where
        (fSplit, sSplit) = splitInput str -- Splits at Entails

-- Merges all expressions into an AND prop expressions because that is where the expression will be split later on
-- Helps to view part before and after entailment as a whole and then makes splitting more efficient
mergeAnd :: [String] -> PropExpr
mergeAnd [] = Empty
mergeAnd [x] = parser x
mergeAnd (x:xs) = parser x :&: mergeAnd xs

strClauseSet :: String ->  [([String],[String])]
strClauseSet str = matchEmpty (getInput str) -- Passes split input to another function to check if one of the expressions is empty

-- Function that only takes into account non-empty prop expressions, applies the cnf Algorithm and then splits them when there is an AND
matchEmpty :: (PropExpr,PropExpr) -> [([String],[String])]
matchEmpty (Empty, Empty) = []
matchEmpty (Empty, a) = splitAnd (cnfAlgorithm a)
matchEmpty (a, Empty) = splitAnd (cnfAlgorithm a)
matchEmpty (a, b) = splitAnd (cnfAlgorithm a) ++ splitAnd (cnfAlgorithm b)

-- Function that applies the cnf algorithm step by step
cnfAlgorithm :: PropExpr -> PropExpr
cnfAlgorithm =
    distribute
    .moveNeg
    .elimImplies
    .elimEqui

-- Eliminates equivalences using Eliminate ⇔ by replacing α ⇔ β with (α ⇒ β) ∧ (β ⇒ α).
elimEqui :: PropExpr -> PropExpr
elimEqui (a :==: b) = elimEqui ((Not a :|: b) :&: (Not b :|: a)) -- Changed case
elimEqui (Not a) = Not (elimEqui a) -- All other cases
elimEqui (a :&: b) = elimEqui a :&: elimEqui b -- All other cases
elimEqui (a :|: b) = elimEqui a :|: elimEqui b -- All other cases
elimEqui (a :->: b) = elimEqui a :->: elimEqui b -- All other cases
elimEqui a =  a -- Base case

-- Eliminates implications using Eliminate ⇒ by replacing α ⇒ β with ¬α ∨ β.
elimImplies :: PropExpr -> PropExpr
elimImplies (a :->: b) = elimImplies (Not a :|: b) -- Changed case
elimImplies (Not a) = Not (elimImplies a) -- All other cases
elimImplies (a :&: b) = elimImplies a :&: elimImplies b -- All other cases
elimImplies (a :|: b) = elimImplies a :|: elimImplies b -- All other cases
elimImplies a = a  -- Base case

-- Eliminating ¬¬ and De Morgan’s laws.
moveNeg :: PropExpr -> PropExpr
moveNeg (Not (Not a)) = moveNeg a -- Changed case
moveNeg (Not (a :&: b)) = moveNeg (Not a :|: Not b) -- Changed case
moveNeg (Not (a :|: b)) = moveNeg (Not a :&: Not b) -- Changed case
moveNeg (Not a) = Not (moveNeg a) -- All other cases
moveNeg (a :&: b) = moveNeg a :&: moveNeg b -- All other cases
moveNeg (a :|: b) = moveNeg a :|: moveNeg b -- All other cases
moveNeg a = a  -- Base case

-- Distribute ∨ over ∧ wherever possible.
distribute :: PropExpr -> PropExpr
distribute (a :|: (b :&: c)) = distribute (a :|: b) :&: distribute (a :|: c) -- Changed case
distribute ((a :&: b) :|: c) = distribute (a :|: c) :&: distribute (b :|: c) -- Changed case
distribute (Not a) = Not (distribute a) -- All other cases
distribute (a :&: b) = distribute a :&: distribute b -- All other cases
distribute (a :|: b) = distribute a :|: distribute b -- All other cases
distribute a = a  -- Base case

-- Splits the modified expressions where there is an AND and formats
splitAnd :: PropExpr -> [([String],[String])]
splitAnd (a :&: b) = splitAnd a ++ splitAnd b -- The PropExpr will be split at all ANDs
splitAnd (a :|: b) = [merge (splitAnd a) (splitAnd b)] -- Merges the different elements of ORs
splitAnd a = [(fposNega,sposNega)] -- Nothing more to split pr merge, base case
    where
        (fposNega, sposNega) = isPosNeg a

-- Adds positives and negatives together
merge :: [([String],[String])] -> [([String],[String])] -> ([String],[String])
merge [(fposNega, sposNega)] [(fposNegb, sposNegb)] = (fposNega++fposNegb, sposNega++sposNegb)

-- Checks if a variable is positive or negative
isPosNeg :: PropExpr -> ([String],[String])
isPosNeg (Not (Variable a)) = ([], [a])
isPosNeg (Variable a) = ([a], [])
isPosNeg _ = ([],[])

----- Conversion -----
intClauseSet :: String -> Clauses
intClauseSet str = noSameClauses (simplify (convert (variables str) (strClauseSet str)))

-- Passes both tuple list of strings to be converted for all tuples
convert :: [(Int, String)] -> [([String],[String])] -> Clauses
convert variables [] = []
convert variables ((fst, snd):xs) = (convertString variables fst, convertString variables snd) : convert variables xs

-- Converts all list of strings of variables to associated list of ints
convertString :: [(Int, String)] -> [String] -> [Int]
convertString variables = map (convertVar variables)

-- make each variable its associated number
convertVar :: [(Int, String)] -> String -> Int
convertVar [] _ =  error "No variable number found"
convertVar ((number, string):xs) var
    | string == var = number
    | otherwise = convertVar xs var

-- reduce all clauses
simplify :: Clauses -> Clauses
simplify [] = []
simplify (x:xs)
    | reduce x == ([],[]) = simplify xs
    | otherwise = reduce x : simplify xs

--  "a clause like [([0,1],[1]) should be removed" - from assignment
reduce :: Clause -> Clause
reduce ([],[]) = ([],[])
reduce (fst, snd) = noPosNeg (noDuplicates fst) (noDuplicates snd) []

-- Removes duplicates within a clause
noDuplicates :: [Int] -> [Int]
noDuplicates [] = []
noDuplicates (x:xs)
    | x `elem` xs = noDuplicates xs
    | otherwise = x : noDuplicates xs

-- Eliminates what is both positive and negative
noPosNeg :: [Int] -> [Int] -> [Int] -> ([Int],[Int])
noPosNeg [] snd fst = (sort fst, sort snd) -- no more variables to be checked
noPosNeg (x:xs) snd fst
    | x `elem` snd = noPosNeg xs (removing x snd) fst -- if an element was in the first list and the second list then it can be removed from both lists
    | otherwise = noPosNeg xs snd (x:fst)

-- removal of the element in the second list
removing :: Int -> [Int] -> [Int]
removing x [] = [] 
removing x (y:ys)
    | x == y = removing x ys
    | otherwise = y : removing x ys

-- removes duplicate clauses
noSameClauses :: Clauses -> Clauses
noSameClauses [] = []
noSameClauses (x:xs)
    | x `elem` xs = noSameClauses xs
    | otherwise = x : noSameClauses xs


----- Resolution -----

-- Does resolutionProof as long as new clauses can be derived
resolutionProof :: Clauses -> Bool
resolutionProof clauses
    | fst && ([],[]) `elem` snd = True
    | fst && not (null(newClauses clauses snd)) = resolutionProof (clauses ++ newClauses clauses snd)
    | otherwise = False
    where
        (fst,snd) = deriveClause clauses

-- Checks if the derived clauses are new
newClauses :: Clauses -> Clauses -> Clauses
newClauses clauses [] = []
newClauses clauses (x:xs)
    | x `notElem` clauses = x : newClauses clauses xs
    | otherwise = newClauses clauses xs

-- Takes one clause at a time and sees what can be derived from it using the others
-- Adds derived clauses when there are some
deriveClause :: Clauses -> (Bool, Clauses)
deriveClause [] = (False, [])
deriveClause (x:xs)
    | fst (tryDeriveFrom x xs) = (True, snd (tryDeriveFrom x xs) ++ snd (deriveClause xs))
    | otherwise = deriveClause xs

-- Tries to derive something with binary comparison of the clause and all the others
-- We do not count when what is derived is just the combination of two clauses
tryDeriveFrom :: Clause -> Clauses -> (Bool, Clauses)
tryDeriveFrom _ [] = (False, [])
tryDeriveFrom (pos1,neg1) ((pos2, neg2):xs)
    | derived /= (sort(pos1++pos2), sort(neg1++neg2)) = (True, derived : snd (tryDeriveFrom (pos1,neg1) xs)) -- sort function used because the elements in the clauses are sorted
    | otherwise = tryDeriveFrom (pos1,neg1) xs
    where
        derived = deriveTwo (pos1,neg1) (pos2, neg2)

-- Tries to combine two clauses
deriveTwo :: Clause -> Clause -> Clause
deriveTwo (pos1,neg1) (pos2, neg2) = noPosNeg (noDuplicates (pos1++pos2)) (noDuplicates (neg1++neg2)) []

-- Main function
propLogProver :: String -> Bool
propLogProver str = resolutionProof (intClauseSet str)
