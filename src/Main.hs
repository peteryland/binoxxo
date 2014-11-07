module Main where

import Types(Row, Grid, QRow, QGrid, transposeQGrid, qGridToGrid, gridToQGrid, parseGrid)
import ValidQRows(validQRows)
import Data.Bits(shiftL, (.&.), (.|.), complement)
import Data.List(foldl', foldl1', (\\))
import Control.Applicative((<$>))
import Control.Monad(liftM, (>=>))
import Control.Monad.Trans(lift)
import Control.Monad.State(StateT, evalStateT, get, put)
import Text.Parsec(parse)

data TryResult a = TR a Bool -- (the result value, has anything changed?)

instance Functor TryResult where
  fmap f (TR x y) =
    TR (f x) y

instance Monad TryResult where
  (TR x changed) >>= f =
    let TR y changed' = f x
    in TR y (changed || changed')
  return x =
    TR x False

setChangedFlag :: Bool -> TryResult a
setChangedFlag = TR undefined

fullMask :: Int -> Int
fullMask len =
  (1 `shiftL` len) - 1

isDone :: Int -> Int -> Bool
isDone len mask =
  mask == fullMask len

tryRow :: Int -> QRow -> StateT [Int] TryResult QRow -- try to solve as much as possible
tryRow len (val, mask) = do
  possibleQRows <- get
  let validRows = filter (\x -> x .&. mask == val) possibleQRows
      eachIs1 = foldl1' (.&.) validRows
      eachIs0 = foldl' (.&.) (fullMask len) (map complement validRows)
      mask' =  eachIs1 .|. eachIs0
      val' = mask' .&. (head validRows)
  if isDone len mask || validRows == [] then
    return (val, mask)
  else do
    -- if you don't want the uniqueness rule, remove the following four lines
    put $ if isDone len mask' then
      possibleQRows \\ validRows
    else
      possibleQRows
    lift . setChangedFlag $ mask /= mask'
    return (val', mask')

tryGridRows :: QGrid -> TryResult QGrid -- try to solve as much as possible
tryGridRows (len, grid) = do
  let completeRows = map fst $ filter (isDone len . snd) grid
      possibleQRows = validQRows len \\ completeRows -- TODO: should use Data.Set
  r <- evalStateT (mapM (tryRow len) grid) possibleQRows
  return (len, r)

tryGridCols :: QGrid -> TryResult QGrid -- as above, but work on cols instead
tryGridCols =
  liftM transposeQGrid . tryGridRows . transposeQGrid

keepTryingBoth :: QGrid -> QGrid
keepTryingBoth =
  mapIf keepTryingBoth . (tryGridRows >=> tryGridCols)
  where
    mapIf f (TR x c) =
      case c of
        True -> f x
        False -> x

solve :: Grid -> Grid
solve =
  qGridToGrid . keepTryingBoth . gridToQGrid

solve' :: String -> String
solve' s =
  case parse parseGrid "(stdin)" s of
    Left e -> "Error parsing:\n" ++ show e ++ "\n"
    Right grid -> show grid ++ "\n\n" ++ (show $ solve grid) ++ "\n"

main :: IO ()
main =
  interact solve'
