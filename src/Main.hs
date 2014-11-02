{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Main where

import Types
import Data.Bits((.&.), (.|.), complement)
import Data.List(foldl', foldl1')
import Control.Applicative((<$>))

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

readGrid :: IO Grid
readGrid = sequence $ replicate 10 $ read <$> getLine

main :: IO ()
main = do grid <- readGrid
          putStrLn $ show grid
          putStrLn $ '\n' : (show $ solve grid)
