module Main where

import Types(Row, Grid, QRow, QGrid, transposeQGrid, qGridToGrid, gridToQGrid, parseGrid)
import ValidQRows(validQRows)
import Data.Bits(shiftL, (.&.), (.|.), complement)
import Data.List(foldl', foldl1')
import Control.Applicative((<$>))
import Text.Parsec(parse)

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
