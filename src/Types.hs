{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Types where

import Data.Bits(shiftL, shiftR, (.&.), (.|.))
import Data.List(intercalate, transpose, foldl', unfoldr)
import Control.Applicative((<$>), (<*))
import Text.ParserCombinators.Parsec(GenParser, (<|>), many, many1, oneOf, newline, digit, eof, char, (<?>))

-- The following are the basic datatypes for the grid

data Cell = U | O | X deriving Eq
type Row = [Cell]
type Grid = [Row]


-- Show instances for our basic types

instance {-# OVERLAPPING #-} Show Cell where
  show c =
    case c of
      O -> "O"
      X -> "X"
      U -> "_"

instance {-# OVERLAPPING #-} Show Row where
  show = concatMap show

instance {-# OVERLAPPING #-} Show Grid where
  show = intercalate "\n" . map show


-- Parser for the grid

parseCell :: GenParser Char st Cell
parseCell =
      (oneOf "xX" >> return X)
  <|> (oneOf "oO" >> return O)
  <|> (char '_' >> return U)
  <?> "x, X, o, O, _"

parseRow :: GenParser Char st Row
parseRow =
  concat <$> many ((:[]) <$> parseCell <|> flip replicate U . read <$> many1 digit) <* newline

parseGrid :: GenParser Char st Grid
parseGrid = do
  r1 <- parseRow
  (r1:) <$> many (do r <- parseRow; return $ r ++ replicate (length r1 - length r) U) <* eof


-- For efficiency, we use a binary representation of the grid when solving.
-- The QRow contains two bit sets: the first is the X/O pattern, the second is the unknown mask.

data QRow = QRow {xbits :: Int, umask :: Int}
data QGrid = QGrid {rowlen :: Int, qrows :: [QRow]}

rowToQRow :: Row -> QRow
rowToQRow =
  foldl' r2qr (QRow 0 0)
  where
    r2qr (QRow xbits umask) x =
      let xbits' = xbits `shiftL` 1
          umask' = umask `shiftL` 1
      in case x of
        U -> QRow xbits' umask'
        O -> QRow xbits' (umask' .|. 1)
        X -> QRow (xbits' .|. 1) (umask' .|. 1)

gridToQGrid :: Grid -> QGrid
gridToQGrid g =
  case g of
    [] -> QGrid 0 []
    r:_ -> QGrid (length r) (map rowToQRow g)

qRowToRow :: Int -> QRow -> Row
qRowToRow len (QRow xbits umask) =
  unfoldr qr2r (1 `shiftL` (len-1))
  where
    qr2r n =
      case n of
        0 -> Nothing
        _ -> let n' = (n `shiftR` 1)
             in case umask .&. n of
               0 -> Just (U, n')
               _ -> case xbits .&. n of
                 0 -> Just (O, n')
                 _ -> Just (X, n')

qGridToGrid :: QGrid -> Grid
qGridToGrid (QGrid len g) =
  map (qRowToRow len) g

transposeQGrid :: QGrid -> QGrid
transposeQGrid =
  gridToQGrid . transpose . qGridToGrid
