{-# OPTIONS -Wall #-}

module Board where

-- | A Board is an 8 x 8 matrix with pieces on it.
data Board a = B [[a]]

instance Show a => Show (Board a) where
    show (B rows) = unlines $ map view rows
                    where view = concat . map (\e -> show e ++ " ")

-- | Each piece represents a different color. Where a piece is missing
--   the color is None.
data Color = White | Black deriving Eq
data Piece = None  | P Color (Int, Int)

instance Show Piece where
    show None        = "-"
    show (P White _) = "w"
    show (P Black _) = "b"

-- | Returns the color of the Piece.
color :: Piece -> Maybe Color
color None        = Nothing
color (P White _) = Just White
color (P Black _) = Just Black

-- | Checks the Color of a Piece.
is :: Color -> Piece -> Bool
is White (P White _) = True
is Black (P Black _) = True
is _ _ = False

-- | Creates a new board.
newboard :: Board Piece
newboard = B $ replicate 8 $ replicate 8 None

-- | Creates a new board from given lists.
fromLists :: [(Int,Int)] -> [(Int,Int)] -> Board Piece
fromLists white black = foldl (\b p -> put b (P White p)) x black
    where x = foldl (\b p -> put b (P Black p)) newboard white

-- | Creates a sample pre-filled board.
sampleBoard :: Board Piece
sampleBoard = fromLists white black
              where white = [(4,2), (6,7), (4,4), (5,6), (2,1)]
                    black = [(0,0), (3,4), (1,7), (6,3), (5,1)]

-- | Creates the standard starting board arrangement.
gameBoard :: Board Piece
gameBoard = fromLists white black
            where white = [(3,3),(4,4)]
                  black = [(4,3),(3,4)]

-- | Colors a piece on board at the given position with the color provided.
colorPiece :: Board Piece -> (Int, Int) -> Color -> Board Piece
colorPiece (B b) (x,y) clr
          | c == Just White = put (B b) (P clr (x,y))
          | c == Just Black = put (B b) (P clr (x,y))
          | otherwise       = (B b) where
          -- extracts the color of a single piece
          c = color $ b !! y !! x

-- | Places a color in the specific location on the board.
put :: Board Piece -> Piece -> Board Piece
put brd None             = brd
put (B b) c@(P _ (x, y)) = B $ u ++ (m:l)
    where (u, r, l) = splitRows b y
          m = column r x c

-- | Partitions the board into upper list of rows, a middle row where
--   the index points to, and a lower list of rows.
splitRows :: [[a]] -> Int -> ([[a]], [a], [[a]])
splitRows rows y = (u, concat m, l)
    where (um, l) = splitAt (y+1) rows
          (u, m) = splitAt y um

-- | Partitions the row into front, middle, and end parts. The middle
--   part is the element to be replaced by the provided color.
column :: [a] -> Int -> a -> [a]
column els x c = front ++ c:end
    where (fm, end) = splitAt (x+1) els
          (front, _) = splitAt x fm

{-- --}
