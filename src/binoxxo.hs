{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

import Data.Bits(shiftL, (.&.), (.|.), complement)
import Data.List(transpose, foldl', foldl1')
import Data.Char(isDigit)
import Control.Applicative((<$>))

data Cell = U | O | X deriving (Read, Enum, Bounded, Eq)
type Row = [Cell]
type Grid = [Row]

instance Show Cell where
  show c = case c of
             O -> "O"
             X -> "X"
             U -> "_"

instance Show Row where
  show row = (concat $ map show row) ++ "\n"

instance Show Grid where
  show grid = concat $ map show grid

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

validQRows :: [Int]
validQRows = [155,171,173,179,181,182,203,205,211,213,214,217,218,299,301,307,309,310,331,333,339,341,342,345,346,357,358,361,362,364,403,405,406,409,410,421,422,425,426,428,434,436,587,589,595,597,598,601,602,613,614,617,618,620,659,661,662,665,666,677,678,681,682,684,690,692,713,714,716,722,724,805,806,809,810,812,818,820,841,842,844,850,852,868] -- length 10

getValidQRow :: QRow -> QRow
getValidQRow (val, mask) =
  case mask of
    1023 -> (val, mask)
    _ -> let vals = filter (\x -> x .&. mask == val) validQRows
             mask' = (foldl1' (.&.) vals) .|. (foldl' (\x -> (x .&.) . complement) 1023 vals)
             val' = mask' .&. (head vals)
         in (val', mask')

gridToQGrid :: Grid -> QGrid
gridToQGrid = map rowToQRow

qGridToGrid :: QGrid -> Grid
qGridToGrid = map qRowToRow

readGrid :: IO Grid
readGrid = sequence $ replicate 10 $ read <$> getLine

transposeQGrid :: QGrid -> QGrid
transposeQGrid = gridToQGrid . transpose . qGridToGrid

tryRow :: QRow -> (QRow, Bool) -- solve as much as possible, True if changed
tryRow (val, mask) =
  let (val', mask') = getValidQRow (val, mask)
  in ((val', mask'), mask /= mask')

tryGrid :: QGrid -> (QGrid, Bool) -- solve as much as possible, True if changed
tryGrid grid =
  let result = map tryRow grid
  in (map fst result, or $ map snd result)

tryGridT :: QGrid -> (QGrid, Bool) -- as above, but work on cols instead
tryGridT grid =
  let (grid', changed) = tryGrid $ transposeQGrid grid
  in (transposeQGrid grid', changed)

tryBoth :: QGrid -> QGrid -- first try cols, then if something changed, try rows, ...
tryBoth grid = case tryGridT grid of
  (grid', False) -> grid'
  (grid', True) -> case tryGrid grid' of
    (grid'', False) -> grid''
    (grid'', True) -> tryBoth grid''

keepTryingBoth :: QGrid -> QGrid -- try rows, then loop until nothing changes
keepTryingBoth grid = tryBoth $ fst $ tryGrid grid

solve :: Grid -> Grid
solve = qGridToGrid . keepTryingBoth . gridToQGrid


main :: IO ()
main = do grid <- readGrid
          putStrLn $ show grid
          putStrLn $ show $ solve grid
