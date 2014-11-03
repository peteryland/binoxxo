module Main where

import Types(Row, Grid, QRow, QGrid, transposeQGrid, qGridToGrid, gridToQGrid, parseGrid)
import ValidQRows(validQRows)
import Data.Bits(shiftL, (.&.), (.|.), complement)
import Data.List(foldl', foldl1', (\\))
import Control.Applicative((<$>))
import Text.Parsec(parse)

fullMask :: Int -> Int
fullMask len =
  (1 `shiftL` len) - 1

isCompleteQRow :: Int -> QRow -> Bool
isCompleteQRow len (_, mask) =
  mask == fullMask len

tryRow :: Int -> [Int] -> QRow -> (QRow, Bool) -- solve as much as possible, True if changed
tryRow len possibleQRows (val, mask) =
  if isCompleteQRow len (val, mask)
  then
    ((val, mask), False)
  else
    let vals = filter (\x -> x .&. mask == val) possibleQRows
        mask' = (foldl1' (.&.) vals) .|. (foldl' (\x -> (x .&.) . complement) (fullMask len) vals)
        val' = mask' .&. (head vals)
    in ((val', mask'), mask /= mask')

tryGrid :: QGrid -> (QGrid, Bool) -- solve as much as possible, True if changed
tryGrid (len, grid) =
  let completeRows = foldl (\x (val, mask) -> if mask == fullMask len then val:x else x) [] grid
      possibleQRows = validQRows len \\ completeRows
      result = map (tryRow len possibleQRows) grid
  in ((len, map fst result), or $ map snd result)

tryGridT :: QGrid -> (QGrid, Bool) -- as above, but work on cols instead
tryGridT grid =
  let (grid', changed) = tryGrid $ transposeQGrid grid
  in (transposeQGrid grid', changed)

keepTryingBoth :: QGrid -> QGrid -- first try rows, then if something changed, try cols, ...
keepTryingBoth grid =
  let (grid', changed') = tryGrid grid
      (grid'', changed'') = tryGridT grid'
  in if changed' || changed'' then keepTryingBoth grid'' else grid''

solve :: Grid -> Grid
solve =
  qGridToGrid . keepTryingBoth . gridToQGrid

solve' :: String -> String
solve' s =
  case parse parseGrid "(stdin)" s of
    Left e -> "Error parsing:\n" ++ show e
    Right grid -> show grid ++ "\n\n" ++ (show $ solve grid) ++ "\n"

main :: IO ()
main =
  interact solve'
