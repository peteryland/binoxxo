{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Main where

import Types

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

validQRows :: [Int]
validQRows = map fst $ gridToQGrid allLegalRows

main :: IO ()
main = putStrLn $ show validQRows
