# SATsolver
Haskell app that verifies satisfiability of given formula.
Formula can be made up of single character named variables, and "implies", "or", "not", "and" operators.
For example:
~~~~
(not p) and (p or not q)
~~~~
All operators are assumed to be right associative, meaning <code>p and q implies r</code> is equivalent to <code>p and (q implies r)</code>
# Compilation
Run <code>ghc Sat.hs</code> in the project directory. <br />
Data.List.Split module is required, to download it, run <code>cabal install --lib --package-env . split</code> in the project directory
# How to use
Run executable, then provide formula as first line input. 
