{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Types where

import Data.Bits(shiftL, shiftR, (.&.), (.|.), complement)
import Data.List(intercalate, transpose, foldl', unfoldr)
import Data.Char(isDigit)
import Control.Applicative((<$>), (<*))
import Text.ParserCombinators.Parsec(GenParser, (<|>), many, many1, oneOf, newline, digit, eof, char)

data Cell = U | O | X deriving (Read, Enum, Bounded, Eq)
type Row = [Cell]
type Grid = [Row]

instance Show Cell where
  show c = case c of
             O -> "O"
             X -> "X"
             U -> "_"

instance Show Row where
  show row =
    concatMap show row

instance Show Grid where
  show grid =
    intercalate "\n" $ map show grid

parseCell :: GenParser Char st Cell
parseCell =
  (oneOf "xX" >> return X) <|> (oneOf "oO" >> return O) <|> (char '_' >> return U)

parseRow :: GenParser Char st Row
parseRow =
  concat <$> many ((:[]) <$> parseCell <|> flip replicate U . read <$> many1 digit) <* newline

parseGrid :: GenParser Char st Grid
parseGrid = do
  r1 <- parseRow
  (r1:) <$> many (do r <- parseRow; return $ r ++ replicate (length r1 - length r) U) <* eof

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

gridToQGrid :: Grid -> QGrid
gridToQGrid = map rowToQRow

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

qGridToGrid :: QGrid -> Grid
qGridToGrid = map qRowToRow

transposeQGrid :: QGrid -> QGrid
transposeQGrid = gridToQGrid . transpose . qGridToGrid
