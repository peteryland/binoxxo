module Main where

import Control.Monad(replicateM)
import Types(Cell(..), Row, gridToQGrid)

hasNoTrips xs =
  case xs of
    (X:X:X:_) -> False
    (O:O:O:_) -> False
    (_:xs')   -> hasNoTrips xs'
    []        -> True

isLegalRow :: Row -> Bool
isLegalRow row =
  let halflen = length row `div` 2
      isodd = length row `mod` 2
  in (countEq X row == halflen + isodd)
     && (countEq O row == halflen)
     && (hasNoTrips row)
  where
    countEq x = length . filter (== x)

allPossibleRows :: Int -> [Row]
allPossibleRows =
  flip replicateM [O, X]

allLegalRows :: Int -> [Row]
allLegalRows =
  filter isLegalRow . allPossibleRows

validQRows :: Int -> [Int]
validQRows =
  map fst . snd . gridToQGrid . allLegalRows

mkTable :: Int -> [Char]
mkTable n =
  "    " ++ show n ++ " -> " ++ (show $ validQRows n)

main :: IO ()
main = do
  putStrLn "module ValidQRows where\n"
  putStrLn "validQRows :: Int -> [Int]"
  putStrLn "validQRows n =\n  case n of"
  mapM_ (putStrLn . mkTable) [1..16]
  putStrLn "    _ -> []"
