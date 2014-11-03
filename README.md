## Binoxxo Solver

You need a Haskell compiler or interpreter to run this.

To compile and run the tests simply type `make`.

The program expects a grid on stdin in any of the following formats, with the
first row setting the row length for the rest of the grid (so it must be a full
row):

```
xo____XO__ (full row, case insensitive)
xo____XO   (end of row assumed to be not given)
xo4XO2     (unknowns replaced with number of them)
xo4XO      (any mix of above)
xo_2_XO    (any mix of above)
__________ (blank row)
_          (blank row)
           (blank row)
0          (blank row)
10         (blank row)
```

This will work with any NxM puzzles, where N,M <= 16.  If you want it to work
with bigger grids, just change src/MkTable.hs to output up to whatever size you
need.
