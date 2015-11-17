{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception(try, evaluate, SomeException)
import Control.Monad(replicateM)
import System.Environment(getArgs)
import Types(Cell(..), Row, gridToQGrid, xbits, qrows)

default_maxLen = 14 -- default value for maxLen

-- Check if a given row meets the rules
isValidRow :: Row -> Bool
isValidRow row =
  let halflen = length row `div` 2
      isodd = length row `mod` 2
  in (countEq X row == halflen + isodd)
     && (countEq O row == halflen)
     && (hasNoTrips row)
  where
    countEq x = length . filter (== x)
    hasNoTrips xs =
      case xs of
        (X:X:X:_) -> False
        (O:O:O:_) -> False
        (_:xs')   -> hasNoTrips xs'
        []        -> True

-- Output a list of all possible rows for a given row length
validQRows :: Int -> [Int]
validQRows =
  map xbits . qrows . gridToQGrid . validRows
  where
    validRows = filter isValidRow . possibleRows
    possibleRows = flip replicateM [O, X]

-- Create the rows for the haskell case statement generated below
mkTable :: Int -> [Char]
mkTable n =
  "    " ++ show n ++ " -> " ++ (show $ validQRows n)

-- Build a haskell source file containing the lists of possible qrow values
main :: IO ()
main = do
  -- the first argument sets the maximum row length this solver can handle
  args <- getArgs
  maxLenE <- try $ evaluate $ read $ head args
  let maxLen = either (\(e::SomeException) -> default_maxLen) id maxLenE
  putStrLn "module ValidQRows where\n"
  putStrLn "validQRows :: Int -> [Int]" -- TODO: why not output sets?
  putStrLn "validQRows n =\n  case n of"
  mapM_ (putStrLn . mkTable) [1..maxLen]
  putStrLn "    _ -> []"
