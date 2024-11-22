# Propositional-Logic-Solver
A resolution-based propositional logic theorem prover using Functional Programming.

This solver was made in the context of the Functional Programming course at the University of Groningen.

## To run
- `ghci`
- `:load propLogSolver.hs`
- `propLogProver "[premises] ENTAILS [goals]"`

Examples:
- propLogProver "[a; a->b; b->c] ENTAILS [c]"
- propLogProver "[a and b] ENTAILS [a]"
- propLogProver "[a or b] ENTAILS [a]"

## Syntax
The logic system accepts the following logical operators and symbols:

- **Negation (~)**
- **Conjunction (AND)**
- **Disjunction (OR)**
- **Implication (->)**
- **Equivalence (==)**
- **Variable**
- **Empty**


## Output
The provided function returns False if the input is invalid or unsatisfiable in the sense of propositional resolution logic.

If the goals can be inferred, then the function returns True.
