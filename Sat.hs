import Parser
import Converter
import Types
import Data.List.Split
import Data.List


nein :: Literal -> Literal
nein (Literal a) = Negated a
nein (Negated a) = Literal a

-- apply value to target literal in clause
inClause :: Clause -> Literal -> Bool -> Clause
inClause (Clause claus) p@(Literal _) True = if p `elem` claus then Tautology
                                    else Clause $ filter (/= nein p) claus
inClause (Clause claus) p@(Literal _) False = if nein p `elem` claus then Tautology
                                    else Clause $ filter (/= p) claus
inClause claus lit val = inClause claus (nein lit) $ not val

-- apply val to literal in every clause
applied :: Cnf -> Literal -> Bool -> Maybe Cnf
applied form p True = if Clause [] `elem` res then Nothing
                        else Just res
    where   res = filter (/= Tautology) $ map (\x -> inClause x p True) $ filter (/= Clause []) form 
applied form p False = applied form (nein p) True

-- get hidden val
extract :: Clause -> [Literal]
extract (Clause a) = a
extract Tautology = []

-- get normalised literal
real :: Literal -> Bool -> (Char, Bool)
real (Literal a) val = (a, val)
real (Negated a) val = (a, not val)

monadWannabe :: Result -> (Char, Bool) -> Result
monadWannabe (Sat xs) pr = Sat (pr:xs)
monadWannabe Unsat _ = Unsat

-- do magic
solve :: Cnf -> Result
solve [] = Sat []
solve (x:xs) = case applied (x:xs) (head $ extract x) True of
        Nothing -> case applied (x:xs) (head $ extract x) False of
            Nothing -> Unsat
            Just sth -> monadWannabe (solve sth) $ real (head $ extract x) False 
        Just sth -> case solve sth of
            Sat b -> Sat (real (head (extract x)) True:b) 
            Unsat -> case applied (x:xs) (head $ extract x) False of
                Nothing -> Unsat
                Just a -> monadWannabe (solve a) $ real (head $ extract x) False


-- ... -> "acd", all used literal names
used :: Cnf -> String
used = unique . map (fst . flip real True) . unnest . map extract
    where unique = map head . group . sort
          unnest arr = [ x | y <- arr, x <- y]

-- the end
sat :: String -> Result
sat frm = case res of
        Unsat -> Unsat
        Sat arr -> Sat (arr ++ [ (x, True) | x <- missing])
            where missing = usedIn \\ map fst arr
    where inp = convert $ parse frm
          usedIn = used inp
          res = solve inp

main = do
    x <- getLine
    print $ sat x
