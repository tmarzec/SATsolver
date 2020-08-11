module Parser (
    parse
) where

import Types

-- "p q not or" -> Or p (not q)
fromRpn :: String -> Formula
fromRpn form = head . foldl folder [Variable $ head form] $ tail $ words form
    where   folder (x:y:ys) "and" = And y x: ys
            folder (x:y:ys) "or" = Or y x : ys
            folder (x:xs) "not" = Not x : xs
            folder (x:y:ys) "implies" = Implies y x : ys
            folder arr [x] = Variable x : arr
            folder _ _ = error "incorrect input ;c"



-- up to first "("
dump :: ([String], [String]) -> ([String], [String])
dump (a, "(" : xs) = (a, xs)
dump (a, x : xs) = dump (x : a, xs)

toRpn :: [String] -> String
toRpn str = (unwords . reverse . fst) (foldl func ([], []) (("(" : str) ++ [")"]))
    where   func (a, b) "(" = (a, "(" : b)
            func (a, b) ")" = dump (a, b)
            func (a, b) [v] = ([v] : a, b)
            func (a, b) op = (a, op : b)


-- "(p) or (q)" -> ["(","p",")","or","(","q",")"]
clean :: String -> [String]
clean = words . foldr func []
    where   func ')' arr = " ) " ++ arr 
            func '(' arr = " ( " ++ arr
            func x xs = x : xs

parse :: String -> Formula
parse = fromRpn . toRpn . clean

{- CNF part

-- "(a,b),(x,y)..." -> ["a,b", "x,y",...]
parseOut :: String -> [String]
parseOut str = head x : map tail (tail x)
    where x = map tail $ endBy ")" str

-- "a" -> Literal 'a', "~a" -> Negated 'a' 
getLiteral :: String -> Literal
getLiteral [a] = Literal a
getLiteral ('~':[a]) = Negated a
getLiteral x = error $ "wrong literal! " ++ x

-- "(a,b),(x,y)" -> [Literal 'a',...]
parseCnf :: String -> Cnf
parseCnf str = map (Clause . map getLiteral . splitOn ",") (parseOut str)

-}

{-
-- ["(", "p", "or", "q", ")"] -> "p q or"
toRpn :: [String] -> String
toRpn strs = unwords $ reverse (if null a then fs else (let [x] = a in x) : fs)
    where   func a "(" = a
            func (acc, x : xs) ")" = (x : acc, xs)
            func (acc, ops) [v] = ([v] : acc, ops)
            func (acc, ops) sth = (acc, sth : ops)
            (fs, a) = foldl func ([], []) strs
-}