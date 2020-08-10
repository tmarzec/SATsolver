module Converter (
    convert
) where

import Types

type Clause' = [Literal]

-- Formula -> CNF
convert' :: Formula -> [Clause']
convert' (Variable x) = [[Literal x]]
convert' (And x y) = convert' x ++ convert' y
convert' (Or x y) = [ a ++ b | a <- convert' x, b <- convert' y]
convert' (Implies x y) = convert' $ Or (Not x) y
convert' (Not a) = case a of
    Variable z -> [[Negated z]]
    And x y -> convert' $ Or (Not x) (Not y)
    Or x y -> convert' $ And (Not x) (Not y)
    Implies x y -> convert' $ And x $ Not y
    Not x -> convert' x

convert :: Formula -> Cnf
convert fm = map Clause (convert' fm)