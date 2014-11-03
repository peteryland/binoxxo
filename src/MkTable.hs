module Main where

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
  in ((length . filter (==X)) row == halflen + isodd)
     && ((length . filter (==O)) row == halflen)
     && (hasNoTrips row)

allLegalRows :: Int -> [Row]
allLegalRows n = filter isLegalRow $ allPossibleRows n
  where
    allPossibleRows 0 = [[]]
    allPossibleRows n =
      let rows' = allPossibleRows $ n-1
      in map (O:) rows' ++ map (X:) rows'

validQRows :: Int -> [Int]
validQRows n =
  map fst $ snd $ gridToQGrid (allLegalRows n)

mkTable :: Int -> [Char]
mkTable n =
  "    " ++ show n ++ " -> " ++ (show $ validQRows n)

main :: IO ()
main = do
  putStrLn "module ValidQRows where\n"
  putStrLn "validQRows :: Int -> [Int]"
  putStrLn "validQRows n =\n  case n of"
  sequence_ $ map (putStrLn . mkTable) [1..16]
  putStrLn "    _ -> []"
