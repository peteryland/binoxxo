{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Main where

import Types
import Data.Bits(shiftL, (.&.), (.|.), complement)
import Data.List(foldl', foldl1')
import Control.Applicative((<$>))
import Text.Parsec(parse)

validQRows :: Int -> [Int]
validQRows 10 = [155,171,173,179,181,182,203,205,211,213,214,217,218,299,301,307,309,310,331,333,339,341,342,345,346,357,358,361,362,364,403,405,406,409,410,421,422,425,426,428,434,436,587,589,595,597,598,601,602,613,614,617,618,620,659,661,662,665,666,677,678,681,682,684,690,692,713,714,716,722,724,805,806,809,810,812,818,820,841,842,844,850,852,868] -- length 10
validQRows _ = []

fullMask :: Int -> Int
fullMask len =
  (1 `shiftL` len) - 1

isCompleteQRow :: Int -> QRow -> Bool
isCompleteQRow len (_, mask) =
  mask == fullMask len

getValidQRow :: Int -> QRow -> QRow
getValidQRow len (val, mask) =
  if isCompleteQRow len (val, mask)
  then
    (val, mask)
  else
    let vals = filter (\x -> x .&. mask == val) (validQRows len)
        mask' = (foldl1' (.&.) vals) .|. (foldl' (\x -> (x .&.) . complement) (fullMask len) vals)
        val' = mask' .&. (head vals)
    in (val', mask')

tryRow :: Int -> QRow -> (QRow, Bool) -- solve as much as possible, True if changed
tryRow len (val, mask) =
  let (val', mask') = getValidQRow len (val, mask)
  in ((val', mask'), mask /= mask')

tryGrid :: QGrid -> (QGrid, Bool) -- solve as much as possible, True if changed
tryGrid (len, grid) =
  let result = map (tryRow len) grid
  in ((len, map fst result), or $ map snd result)

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
solve =
  qGridToGrid . keepTryingBoth . gridToQGrid

solve' :: String -> String
solve' s =
  case parse parseGrid "(stdin)" s of
    Left e -> "Error parsing:\n" ++ show e
    Right grid' -> show grid' ++ "\n\n" ++ (show $ solve grid') ++ "\n"

main :: IO ()
main =
  interact solve'
