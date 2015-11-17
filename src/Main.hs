module Main where

import Types(Row, Grid, QRow(..), QGrid(..), transposeQGrid, qGridToGrid, gridToQGrid, parseGrid)
import ValidQRows(validQRows)
import Data.Bits(shiftL, (.&.), (.|.), complement)
import Data.List(foldl', foldl1', (\\))
import Control.Applicative((<$>), Applicative(..))
import Control.Monad(liftM, (>=>), ap)
import Control.Monad.Trans(lift)
import Control.Monad.State(StateT, evalStateT, get, put)
import Text.Parsec(parse)

-- Create a monad that will track if we're making any progress
data ChangeMonitor a = ChangeMonitor { val :: a, changed :: Bool }

instance Monad ChangeMonitor where
  (ChangeMonitor x changed) >>= f =
    let ChangeMonitor y changed' = f x
    in ChangeMonitor y (changed || changed')
  return = pure

instance Applicative ChangeMonitor where
  pure x = ChangeMonitor x False
  (<*>) = ap

instance Functor ChangeMonitor where
  fmap = liftM

allOnes len = (1 `shiftL` len) - 1
allKnown len umask = umask == allOnes len

-- try to solve as much as possible
solveRow :: Bool -> Int -> QRow -> StateT [Int] ChangeMonitor QRow
solveRow useUniqueRule len (QRow xbits umask) = do
  possibleQRows <- get
  let validRows = filter (\x -> x .&. umask == xbits) possibleQRows
      eachIs1 = foldl1' (.&.) validRows
      eachIs0 = foldl' (.&.) (allOnes len) (map complement validRows)
      umask' =  eachIs1 .|. eachIs0
      xbits' = umask' .&. (head validRows)
  if allKnown len umask || validRows == [] then
    return (QRow xbits umask)
  else do
    put $ if useUniqueRule && allKnown len umask' then
        possibleQRows \\ validRows
      else
        possibleQRows
    setChanged $ umask /= umask'
    return (QRow xbits' umask')
  where
    setChanged = lift . ChangeMonitor undefined

-- try to solve as much as possible
solveRows :: Bool -> QGrid -> ChangeMonitor QGrid
solveRows useUniqueRule (QGrid len grid) = do
  let completeRows = map xbits $ filter (allKnown len . umask) grid
      possibleQRows = if useUniqueRule then validQRows len \\ completeRows -- TODO: should use Data.Set
                                  else validQRows len
  r <- evalStateT (mapM (solveRow useUniqueRule len) grid) possibleQRows
  return (QGrid len r)

-- as above, but work on cols instead
solveCols :: Bool -> QGrid -> ChangeMonitor QGrid
solveCols useUniqueRule =
  liftM transposeQGrid . solveRows useUniqueRule . transposeQGrid

solveQGrid :: Bool -> QGrid -> QGrid
solveQGrid useUniqueRule =
  mapIf (solveQGrid useUniqueRule) . (solveRows useUniqueRule >=> solveCols useUniqueRule)
  where
    mapIf f (ChangeMonitor x c) =
      case c of
        True -> f x
        False -> x

solve :: Bool -> Grid -> Grid
solve useUniqueRule =
  qGridToGrid . solveQGrid useUniqueRule . gridToQGrid

parseAndSolve :: Bool -> String -> String
parseAndSolve useUniqueRule s = case parse parseGrid "(stdin)" s of
  Left e -> "Error parsing:\n" ++ show e ++ "\n"
  Right grid -> show grid ++ "\n\n" ++ (show $ solve useUniqueRule grid) ++ "\n"

main :: IO ()
main =
  let useUniqueRule = True
  in interact $ parseAndSolve useUniqueRule
