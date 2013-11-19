{-# OPTIONS -Wall #-}

module Action where

import Board
import Data.Maybe
import Data.List

-- | The Player type.
data Player = WhiteP | BlackP deriving Eq

-- | A Game is a Board and a next Player.
data Game = Game { board :: Board Piece, onTurn :: Player }
instance Show Game where
    show (Game b _) = show b

-- | Creates a new game.
newgame :: IO Game
newgame = return $ Game gameBoard BlackP

-- TO IMPLEMENT: decides whether a player can make a move in the
-- current configuration

checkIfEmpty :: Piece -> Bool
checkIfEmpty None = True
checkIfEmpty _    = False

getAdjacentSquares :: Board Piece -> Piece -> [(Int, Int)]
getAdjacentSquares _ None = []
getAdjacentSquares (B b) (P White (y,x)) =
  [ p | i <- [x-1..x+1], j <- [y-1..y+1],
    let p = (j, i), i >= 0, j >= 0, i < 8, j < 8]
getAdjacentSquares (B b) (P Black (y,x)) =
  [ p | i <- [x-1..x+1], j <- [y-1..y+1],
    let p = (j, i), i >= 0, j >= 0, i < 8, j < 8]

--checkIfAdjacent :: Player -> Piece -> [Piece] -> Bool
--checkIfAdjacent WhiteP (Black _) l = True
--checkIfAdjacent BlackP (White _) l = True
--checkIfAdjacent _ _ _              = False

emptySpaces :: Game -> [(Int, Int)]
emptySpaces Game{board=(B b)} =
  [(i, j) | i <- [0..7], j <- [0..7], checkIfEmpty (b !! j !! i)]

adjacentSpaces :: Game -> [(Int, Int)]
adjacentSpaces Game{board=(B b), onTurn=p} = allAdj (B b) 7 7

allAdj :: Board Piece -> Int -> Int -> [(Int, Int)]
allAdj a@(B b) 0 j = getAdjacentSquares a (b !! j !! 0)
allAdj a@(B b) i 0 = getAdjacentSquares a (b !! 0 !! i)
allAdj a@(B b) j i =
  (getAdjacentSquares a (b !! j !! i)) ++
   (allAdj a (j - 1) i) ++ (allAdj a j (i - 1))

{-
acrossSpaces :: Game -> Int -> Int -> [(Int, Int)]
acrossSpaces g 0 i = eachSpace g 0 i
acrossSpaces g j 0 = eachSpace g j 0
acrossSpaces g j i = (eachSpace g j i) ++ (acrossSpaces g (j - 1) i) ++
                     (acrossSpaces g j (i - 1))

eachSpace :: Game -> Int -> Int -> [(Int, Int)]
eachSpace g j i = (ruleValidSpaces (+0) (+1) (j, i) g) ++
                  (ruleValidSpaces (+0) (\b -> b - 1) (j, i) g) ++
                  (ruleValidSpaces (+1) (+0) (j, i) g) ++
                  (ruleValidSpaces (\b -> b - 1) (+0) (j, i) g) ++
                  (ruleValidSpaces (+1) (+0) (j, i) g) ++
                  (ruleValidSpaces (+1) (+1) (j, i) g) ++
                  (ruleValidSpaces (+1) (\b -> b - 1) (j, i) g) ++
                  (ruleValidSpaces (\b -> b - 1) (+1) (j, i) g) ++
                  (ruleValidSpaces (\b -> b - 1) (\b -> b - 1) (j, i) g)
-}

-- Special across rule
-- 1. Initial position is empty
-- 2. From initial position go in all directions
-- 3. Make sure adj is opposite color
-- 4. Traverse until find same color

specPos :: (Int, Int) -> Game -> Bool
specPos c@(j, i) g@(Game (B b) p) = checkIfEmpty (b !! j !! i)
                                    && (checkEachSpace c g)

checkEachSpace :: (Int, Int) -> Game -> Bool
checkEachSpace (j, i) g = (ruleValidSpaces (+0) (+1) (j, i) g) ||
                          (ruleValidSpaces (+0) (\b -> b - 1) (j, i) g) ||
                          (ruleValidSpaces (+1) (+0) (j, i) g) ||
                          (ruleValidSpaces (\b -> b - 1) (+0) (j, i) g) ||
                          (ruleValidSpaces (+1) (+1) (j, i) g) ||
                          (ruleValidSpaces (+1) (\b -> b - 1) (j, i) g) ||
                          (ruleValidSpaces (\b -> b - 1) (+1) (j, i) g) ||
                          (ruleValidSpaces (\b -> b - 1) (\b -> b - 1) (j, i) g)

ruleValidSpaces :: (Int -> Int) -> (Int -> Int) -> (Int, Int) -> Game -> Bool
ruleValidSpaces f k (j, i) g@(Game (B b) p)
  -- out of bounds
  | j < 0 || j > 7 || i < 0 || i > 7         = False
  | p == WhiteP
    && color (b !! f j !! k i) == Just Black = ruleValidSpaces f k (f j, k i) g
  | p == BlackP
    && color (b !! f j !! k i) == Just White = ruleValidSpaces f k (f j, k i) g
  | p == WhiteP && color (b !! f j !! k i) == Just White
    && color (b !! j !! i) == Just Black     = True
  | p == BlackP && color (b !! f j !! k i) == Just Black
    && color (b !! j !! i) == Just White     = True
  | otherwise                                        = False

--checkValidRule 

validMoves :: Game -> [(Int, Int)]
--validMoves _ = [(0,0), (1,1)]
validMoves g@(Game brd plr) = (emptySpaces g) `intersect` (adjacentSpaces g)
                              --`intersect` (acrossSpaces g 7 7)


-- | Checks whether the current (x,y) location can be played
--   by the player on turn. (x,y) indices start at 0.
legalMove :: Game -> (Int, Int) -> Bool
legalMove g (x, y) = ((x, y) `elem` validMoves g) && specPos (y, x) g

takePieces :: Board Piece -> (Int, Int) -> Maybe Color -> Board Piece
takePieces brd p c =
    let up    = convert brd brd p ((+) 0, (+) (-1)) c in
    let down  = convert up up p ((+) 0, (+) 1) c in
    let left  = convert down down p ((+) (-1), (+) 0) c in
    let right = convert left left p ((+) 1, (+) 0) c in
    let upl   = convert right right p ((+) (-1), (+) (-1)) c in
    let downl = convert upl upl p ((+) (-1), (+) 1) c in
    let upr   = convert downl downl p ((+) 1, (+) (-1)) c in
    let downr = convert upr upr p ((+) 1, (+) 1) c in
    downr

-- | Converts the pieces on the board starting from the provided
--   coordinate and moving along the functions by flipping their
--   colors into the color given.
convert :: Board Piece -> Board Piece -> (Int, Int) -> (Int -> Int, Int -> Int)
        -> Maybe Color -> (Board Piece)
convert b@(B brd) acc (x,y) (fx,fy) c
        -- out of bounds
        | fx x < 0 || fx x > 7 || fy y < 0 || fy y > 7 = b
        -- no more in that direction
        | color (brd !! (fy y) !! (fx x)) == Nothing   = b
        -- reached a limiting value
        | color (brd !! (fy y) !! (fx x)) == c         = acc
        -- flip the piece
        | otherwise = convert b nb (fx x, fy y) (fx,fy) c
        where nb = colorPiece acc (fx x, fy y) (fromJust c)

-- | Plays a move in the game on the coordinates specified.
play :: Game -> (Int, Int) -> Game
play g@(Game brd plr) p
     | legalMove g p = Game tb (nplr plr)
     | otherwise     = g where
     -- the updated board
     nb = put brd np
     np = piece plr p
     tb = takePieces nb p (color np)
     -- the next player
     nplr WhiteP = BlackP
     nplr BlackP = WhiteP

-- determines the piece to be played by the player
piece :: Player -> (Int, Int) -> Piece
piece WhiteP = P White
piece BlackP = P Black

{-- --}
