## Binoxxo Solver

You need a Haskell compiler or interpreter to run this.

To compile and run the tests simply type `make`.

The program expects a grid on stdin in any of the following formats:

`xo____XO__` (full row, case insensitive)
`xo____XO  ` (end of row assumed to be unknown)
`xo4XO2    ` (unknowns replaced with number of them)
`xo4XO     ` (any mix of above)
`xo_2_XO   ` (any mix of above)
`__________` (blank row)
`_         ` (blank row)
`          ` (blank row)
`0         ` (blank row)
`10        ` (blank row)

This will only work with 10x10 puzzles, which are the only ones I've seen so
far.  It can be modified fairly trivially for NxN, and only a little less
trivially for NxM.
