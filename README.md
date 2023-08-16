# Palisade Solver

Solves [Palisade](https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/palisade.html) puzzles by reduction to SAT and use of [minisat-solver](https://hackage.haskell.org/package/minisat-solver).

The first 3 numbers in the examples are width heigh and region size. The remander of the file contains the constraints. Any non-digit charachters can be used to indicate lack of a constraint for some cell and lines can end early when the last constraint in a line is not on the edge.
