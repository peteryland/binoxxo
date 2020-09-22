## Binoxxo and Binairo Solver
[![Travis Build Status](https://travis-ci.org/peteryland/binoxxo.svg?branch=master)](https://travis-ci.org/peteryland/binoxxo)

This program solves *Binoxxo* and *Binairo* (aka *Tohu wa Vohu*, *Binary
Puzzle*, *Tic-Tac-Logic*, *Unruly*, *Takuzu*) puzzles.  The Rules: From a
partially-filled grid of Xs and Os (or sometimes 0s and 1s), the grid must be
filled in, ensuring that no three symbols appear consecutively, and that each
row and column contains an equal number of each symbol.  In some publications
the uniqueness rule applies, which adds the extra condition that all rows and
columns must be unique.  This solver honours this extra rule by default.

You need a Haskell compiler or interpreter to run the solver.  Get it from your
package manager (e.g. `aptitude install haskell-platform`), or from the
[Haskell homepage](https://www.haskell.org/platform/).  If you have ghc but not
the full Haskell Platform for your system (e.g. on a Raspberry Pi), you'll also
need to install parsec (`cabal install parsec`).

To compile and run the tests simply type `make`.  It takes around 6s to build
and run all the tests on an m1.micro instance.  At first glance, it might look
like an `O(n)` solver for `n`x`n` puzzles, but the transpose operation is the
expensive one, turning it into a worst case `O(n)` but normally very close to
`O(1)` algorithm.

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

For example:

```
x3x5
3o
oo3o2x
1o2x3o
4x2x
9o
1o4ox
7x
1oo3o
2x2o1x
```

Please see the test directory for more examples.

This will work out of the box with any `N⨉M` puzzles, where `1 ≤ N,M ≤ 14`.  If
you want it to work with bigger grids, build with `make MAXLEN=`*`len`*,
setting *`len`* to whatever size you need.

This program is Copyright (c) 2015-2020 Pete Ryland and licensed under the GNU
GPLv3.
