{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

import Data.Bits(bit, setBit, clearBit, testBit, shiftL, shiftR, (.&.), (.|.), complement)
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

isLegalGrid :: Grid -> Bool
isLegalGrid grid = rowsLegal grid && colsLegal grid && rowsUnique grid && colsUnique grid
  where
    rowsLegal grid = and $ map isLegalRow grid
    rowsUnique (row:rows) = (U `elem` row || row `notElem` rows) && rowsUnique rows
    rowsUnique []         = True
    colsLegal  grid = rowsLegal  $ transpose grid
    colsUnique grid = rowsUnique $ transpose grid

type GNums = (Int, Int)

rowToGNums :: Row -> GNums
rowToGNums row = foldl' r2n (0, 0) row
  where
    r2n (val, mask) x =
      let (val', mask') = (val `shiftL` 1, mask `shiftL` 1)
      in case x of
        U -> (val', mask')
        O -> (val', mask' .|. 1)
        X -> (val' .|. 1, mask' .|. 1)

gNumsToRow :: GNums -> Row
gNumsToRow (val, mask) = n2r [] 1
  where
    n2r row n = case n of
      1024 -> row
      _ -> case mask .&. n of
        0 -> n2r (U:row) (n `shiftL` 1)
        _ -> case val .&. n of
          0 -> n2r (O:row) (n `shiftL` 1)
          _ -> n2r (X:row) (n `shiftL` 1)

validGNumVals :: [Int]
-- validGNumVals = map fst $ gridToGNums allLegalRows
validGNumVals = [155,171,173,179,181,182,203,205,211,213,214,217,218,299,301,307,309,310,331,333,339,341,342,345,346,357,358,361,362,364,403,405,406,409,410,421,422,425,426,428,434,436,587,589,595,597,598,601,602,613,614,617,618,620,659,661,662,665,666,677,678,681,682,684,690,692,713,714,716,722,724,805,806,809,810,812,818,820,841,842,844,850,852,868] -- length 10

isValidGNums :: GNums -> Bool
isValidGNums (val, mask) = val `elem` (map (mask .&.) validGNumVals)

getValidGNums :: GNums -> GNums
getValidGNums (val, mask) =
  case mask of
    1023 -> (val, mask)
    _ -> let vals = filter (\x -> x .&. mask == val) validGNumVals
             mask' = (foldl1' (.&.) vals) .|. (foldl' (\x -> (x .&.) . complement) 1023 vals)
             val' = mask' .&. (head vals)
         in (val', mask')

gridToGNums :: Grid -> [GNums]
gridToGNums = map rowToGNums

gNumsToGrid :: [GNums] -> Grid
gNumsToGrid = map gNumsToRow

readGrid :: IO Grid
readGrid = sequence $ replicate 10 $ read <$> getLine

transposeGNums :: [GNums] -> [GNums]
transposeGNums = gridToGNums . transpose . gNumsToGrid

tryG1r :: GNums -> (GNums, Bool)
tryG1r (val, mask) =
  let (val', mask') = getValidGNums (val, mask)
  in ((val', mask'), mask /= mask')

oldtryG1r :: GNums -> (GNums, Bool)
oldtryG1r (val, mask) = tryG1r' 1 (val, mask)
  where
    tryG1r' t (val, mask)
      | t == 1024
          = ((val, mask), False)
      | mask .&. t /= 0
          = tryG1r' (t `shiftL` 1) (val, mask)
      | not $ isValidGNums (val .|. t, mask .|. t)
          = (fst $ tryG1r' (t `shiftL` 1) (val, mask .|. t), True)
      | not $ isValidGNums (val, mask .|. t)
          = (fst $ tryG1r' (t `shiftL` 1) (val .|. t, mask .|. t), True)
      | otherwise
          = tryG1r' (t `shiftL` 1) (val, mask)

tryG :: [GNums] -> ([GNums], Bool)
tryG gnums = (map fst r, or $ map snd r)
  where r = map tryG1r gnums

tryGT :: [GNums] -> ([GNums], Bool)
tryGT gnums = (transposeGNums r, dS)
  where (r, dS) = tryG $ transposeGNums gnums

tryGBoth :: [GNums] -> ([GNums], Bool)
tryGBoth gnums = (r2, dS1 && dS2)
  where
    (r1, dS1) = tryG gnums
    (r2, dS2) = tryGT r1

keepTryingBoth :: [GNums] -> [GNums]
keepTryingBoth gnums
    | not dS    = r
    | otherwise = keepTryingBoth r
  where (r, dS) = tryGBoth gnums

solve' :: Grid -> Grid
solve' = gNumsToGrid . keepTryingBoth . gridToGNums


main :: IO ()
main = do grid <- readGrid
          putStrLn $ show grid
          putStrLn $ show $ solve' grid
