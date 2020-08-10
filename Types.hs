module Types where

data Literal = Literal Char | Negated Char deriving(Eq, Show)

data Clause = Clause [Literal] | Tautology deriving(Eq, Show)-- x or y
type Cnf = [Clause] -- (x or y) and (x or ~y)

data Result = Sat [(Char, Bool)] | Unsat deriving(Show)

data Formula = Variable Char | And Formula Formula | Or Formula Formula | Implies Formula Formula | Not Formula deriving(Show)
