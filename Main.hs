{-# OPTIONS -Wall #-}

module Main where

import Action
import Board
import Control.Monad

import Graphics.UI.WX hiding (play)
import Graphics.UI.WXCore.WxcClassTypes


-------------------------------------
-- DRAWING UTILITIES ----------------
-------------------------------------

-- | Reads an image from provided filepath.
imageFromFile :: FilePath -> Image ()
imageFromFile fp = image fp

-- | Draws the background.
drawBG :: DC a -> IO ()
drawBG dc = drawImage dc (imageFromFile "reversi-bg.jpg") (Point 0 0) []

-- | Paints the glow around the Piece denoting
--   the player on turn.
drawOnTurn :: DC a -> Player -> IO ()
drawOnTurn dc p = drawImage dc (imageFromFile "glow.png") (pnt p) []
                  where pnt WhiteP = Point 213 468
                        pnt BlackP = Point 313 468

-- | Draws the score to the screen.
drawScore :: DC a -> Board Piece -> IO ()
drawScore dc (B pcs) = do
  drawText dc (show numW) wpnt []
  drawText dc (show numB) bpnt []
      where numW = length $ filter (is White) allP
            numB = length $ filter (is Black) allP
            allP = concat pcs
            wpnt = Point 305 510
            bpnt = Point 405 510

-- | Draws background and board.
drawMap :: Var (IO Game) -> DC a -> Rect -> IO ()
drawMap viog dc _ = do
    iog <- (varGet viog)
    (Game b p) <- iog
    drawBG dc
    drawBoard dc b
    drawOnTurn dc p
    drawScore dc b

-- a helper for converting x-coordinate into x block index
ix :: Int -> Int
ix x = (x-40) `div` 50

-- a helper for converting y-coordinate into y block index
iy :: Int -> Int
iy y = (y-80) `div` 50


-------------------------------------
-- MOUSE CLICK ACTIONS --------------
-------------------------------------

-- | Draws the given board on the screen.
drawBoard :: DC a -> Board Piece -> IO ()
drawBoard dc (B rows) = foldM drawRow () rows where
    -- draws a row of pieces on the screen
    drawRow = foldM drawPc

    -- draws a piece on the screen
    drawPc :: () -> Piece -> IO ()
    drawPc bc None = return bc
    drawPc _ (P White p) = drawImage dc (imageFromFile "white.png") (pnt p) []
    drawPc _ (P Black p) = drawImage dc (imageFromFile "black.png") (pnt p) []

    -- converts a tuple of coordinates into a WX.Point
    pnt :: (Int, Int) -> Point
    pnt (x, y) = Point (40 + 50*x) (80 + 50*y)

-- | Updates the state of the game on a mouse click.
onClick :: Var (IO Game) -> Panel () -> Point -> IO ()
onClick viog p (Point x y) = do
  iog <- varGet viog
  g <- iog
  _ <- varUpdate viog (\_ -> return $ play g (ix x, iy y))
  set p [on paint := drawMap viog]
  repaint p


-------------------------------------
-- MOUSE MOVEMENT ACTIONS -----------
-------------------------------------

-- | Draws the "selected" (yellow) square on the board.
drawSelected :: Var (IO Game) -> Point -> DC a -> Rect -> IO ()
drawSelected viog pnt dc view = do
  drawMap viog dc view
  drawImage dc (imageFromFile "selected.png") (newp pnt) [] where

  -- computes the point where the image should be drawn
  newp (Point x y) = Point (nx x) (ny y)
  nx x = (40 + (x-40) `div` 50 * 50)
  ny y = (80 + (y-80) `div` 50 * 50)


-- | Captures the mouse movement action.
onMouseMove :: Var (IO Game) -> Panel () -> Point -> IO ()
onMouseMove viog p pnt = do
  iog <- varGet viog
  g <- iog
  if legal pnt g then set p [on paint := drawSelected viog pnt]
  else set p [on paint := drawMap viog]
  repaint p  where

  -- computes bounds
  legal (Point x y) g = withinBounds x y && legalMove g (ix x, iy y)
  withinBounds x y = x >= 40 && x <= 440 && y >= 80 && y <= 480


-------------------------------------
-- ACTION CONTROLS ------------------
-------------------------------------

-- | Captures all actions performed with the mouse.
onMouse :: Var (IO Game) -> Panel () -> EventMouse -> IO ()
onMouse viog p (MouseMotion pnt _) = onMouseMove viog p pnt
onMouse viog p (MouseLeftDown pnt _) = onClick viog p pnt
onMouse _ _ _ = return ()


-------------------------------------
-- GRAPHICAL USER INTERFACE ---------
-------------------------------------

gui :: IO ()
gui = do
  -- creates a new game
  g <- varCreate newgame

  -- create the user frame
  f <- frame [text := "wxReversi", layout := space 480 560]

  -- create the panel to draw in
  p <- panel f [on paint := drawMap g, layout := space 480 560]

  -- create a timer that updates stuff
  t <- timer f [interval := 20]

  -- react on user input
  set p [on mouse := onMouse g p]

main :: IO ()
main = start gui
