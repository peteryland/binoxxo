{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Types where

import Data.Bits(shiftL, (.&.), (.|.), complement)
import Data.List(transpose, foldl')
import Data.Char(isDigit)

data Cell = U | O | X deriving (Read, Enum, Bounded, Eq)
type Row = [Cell]
type Grid = [Row]

instance Show Cell where
  show c = case c of
             O -> "O"
             X -> "X"
             U -> "_"

instance Show Row where
  show row = concatMap show row ++ "\n"

instance Show Grid where
  show grid = concatMap show grid

instance Read Row where
  readsPrec _ "" = []
  readsPrec _ input = readsPrec' [] input
    where
      readsPrec' row (x:xs)
        | isDigit x = readsPrec' (row ++ replicate (read $ takeWhile isDigit (x:xs)) U) xs
        | x == '_'  = readsPrec' (row ++ [U]) xs
        | x == 'O'  = readsPrec' (row ++ [O]) xs
        | x == 'X'  = readsPrec' (row ++ [X]) xs
        | x == 'o'  = readsPrec' (row ++ [O]) xs
        | x == 'x'  = readsPrec' (row ++ [X]) xs
      readsPrec' row xs = [(row ++ replicate (10 - length row) U, xs)]


type QRow = (Int, Int) -- (bit set of X/O, mask for unknowns)
type QGrid = [QRow]

rowToQRow :: Row -> QRow
rowToQRow row = foldl' r2qr (0, 0) row
  where
    r2qr (val, mask) x =
      let (val', mask') = (val `shiftL` 1, mask `shiftL` 1)
      in case x of
        U -> (val', mask')
        O -> (val', mask' .|. 1)
        X -> (val' .|. 1, mask' .|. 1)

gridToQGrid :: Grid -> QGrid
gridToQGrid = map rowToQRow

qRowToRow :: QRow -> Row
qRowToRow (val, mask) = qr2r [] 1
  where
    qr2r row n = case n of
      1024 -> row
      _ -> case mask .&. n of
        0 -> qr2r (U:row) (n `shiftL` 1)
        _ -> case val .&. n of
          0 -> qr2r (O:row) (n `shiftL` 1)
          _ -> qr2r (X:row) (n `shiftL` 1)

qGridToGrid :: QGrid -> Grid
qGridToGrid = map qRowToRow

transposeQGrid :: QGrid -> QGrid
transposeQGrid = gridToQGrid . transpose . qGridToGrid
