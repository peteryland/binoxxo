## Binoxxo and Binairo Solver

This program solves *Binoxxo* and *Binairo* (aka *Tohu wa Vohu*, *Binary
Puzzle*, *Tic-Tac-Logic*, *Unruly*) puzzles.  These puzzles are really similar,
differing only in the uniqueness rule.  This solver now applies this rule.
Confusingly, I first saw this puzzle in the Swiss German newspaper [*Blick am
Abend*](http://www.blickamabend.ch/) when it replaced *Hashiwokakero* (aka
*Bridges*, *橋をかけろ*) in early 2014, but they call it *Binoxxo* even though
it has the extra *Binairo* rule.

You need a Haskell compiler or interpreter to run the solver.  Get it from your
package manager (e.g. `aptitude install haskell-platform`), or from the
[Haskell homepage](https://www.haskell.org/platform/).  If you have ghc but not
the full Haskell Platform for your system (e.g. on a Raspberry Pi), you'll also
need to install parsec (`cabal install parsec`).

To compile and run the tests simply type `make`.

The program expects a grid on stdin in any of the following formats, with the
first row setting the row length for the rest of the grid (so it must be a full
row):

```
xo____XO__ (full row, case insensitive)
xo____XO   (rest of row assumed to be blanks)
xo4XO2     (consecutive blanks can be replaced with a number)
xo4XO      (rest of row assumed to be blanks)
xo_2_XO    (any combination of the above rules is valid)
__________ (blank row)
_          (blank row)
           (blank row)
0          (blank row)
10         (blank row)
```

This will work out of the box with any `N⨉M` puzzles, where `1 ≤ N,M ≤ 16`.  If
you want it to work with bigger grids, just change src/MkTable.hs to output up
to whatever size you need.
