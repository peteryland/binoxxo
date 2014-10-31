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

hasNoTrips (X:X:X:xs) = False
hasNoTrips (O:O:O:xs) = False
hasNoTrips (x:xs)     = hasNoTrips xs
hasNoTrips []         = True

isLegalRow :: Row -> Bool
isLegalRow row = ((length . filter (==X)) row <= 5)
              && ((length . filter (==O)) row <= 5)
              && (hasNoTrips row)

allLegalRows :: [Row]
allLegalRows = filter isLegalRow [[a,b,c,d,e,f,g,h,i,j]
    | a <- [O, X], b <- [O, X], c <- [O, X], d <- [O, X], e <- [O, X]
    , f <- [O, X], g <- [O, X], h <- [O, X], i <- [O, X], j <- [O, X]]

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
validQRows = map fst $ gridToQGrid allLegalRows
-- validQRows = [155,171,173,179,181,182,203,205,211,213,214,217,218,299,301,307,309,310,331,333,339,341,342,345,346,357,358,361,362,364,403,405,406,409,410,421,422,425,426,428,434,436,587,589,595,597,598,601,602,613,614,617,618,620,659,661,662,665,666,677,678,681,682,684,690,692,713,714,716,722,724,805,806,809,810,812,818,820,841,842,844,850,852,868] -- length 10

isValidQRow :: QRow -> Bool
isValidQRow (val, mask) = val `elem` (map (mask .&.) validQRows)

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

newtryG1r :: QRow -> (QRow, Bool)
newtryG1r (val, mask) =
  let (val', mask') = getValidQRow (val, mask)
  in ((val', mask'), mask /= mask')

tryG1r :: QRow -> (QRow, Bool)
tryG1r (val, mask) = tryG1r' 1 (val, mask)
  where
    tryG1r' t (val, mask)
      | t == 1024
          = ((val, mask), False)
      | mask .&. t /= 0
          = tryG1r' (t `shiftL` 1) (val, mask)
      | not $ isValidQRow (val .|. t, mask .|. t)
          = (fst $ tryG1r' (t `shiftL` 1) (val, mask .|. t), True)
      | not $ isValidQRow (val, mask .|. t)
          = (fst $ tryG1r' (t `shiftL` 1) (val .|. t, mask .|. t), True)
      | otherwise
          = tryG1r' (t `shiftL` 1) (val, mask)

tryG :: QGrid -> (QGrid, Bool)
tryG grid = (map fst r, or $ map snd r)
  where r = map tryG1r grid

tryGT :: QGrid -> (QGrid, Bool)
tryGT grid = (transposeQGrid r, dS)
  where (r, dS) = tryG $ transposeQGrid grid

tryGBoth :: QGrid -> (QGrid, Bool)
tryGBoth grid = (r2, dS1 && dS2)
  where
    (r1, dS1) = tryG grid
    (r2, dS2) = tryGT r1

keepTryingBoth :: QGrid -> QGrid
keepTryingBoth grid
    | not dS    = r
    | otherwise = keepTryingBoth r
  where (r, dS) = tryGBoth grid

solve :: Grid -> Grid
solve = qGridToGrid . keepTryingBoth . gridToQGrid


main :: IO ()
main = do grid <- readGrid
          putStrLn $ show grid
          putStrLn $ show $ solve grid
