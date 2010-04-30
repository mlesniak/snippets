-- Game of Life in Haskell for an infinite area. Not the most efficient
-- version, but conceptually very easy to understand.
--
module Main where
import Data.List
import Control.Monad
import System.Random


type Position = (Integer, Integer)
type Board    = [Position] -- list of alive cells


-- Simple board for experimentation which cycles. To try it out, type
--   animate (-3,3) (3,-3) b1
b1 = [(0,0), (1,0), (2,0)]


-- Create a board, where cells in the specified area are randomly alive.
-- randomBoard :: Position -> Position -> Int -> IO Board
randomBoard (x1,y1) (x2,y2) num = do
    gen <- getStdGen
    return $ take num $ nub $ makePairs (randoms gen)
  where makePairs (x:y:xs) = 
            let x' = x `mod` (x2-x1+1) + x1
                y' = y `mod` (y1-y2+1) + y2
            in (x',y') : makePairs xs


-- Animate the game by waiting on a keypress before showing the next
-- iteration.
animate :: Position -> Position -> Board -> IO ()
animate ul lr b = do
    showBoard ul lr b
    getLine
    animate ul lr (step b)


-- Show part of the (infinite) board. The coordinates determine the upper left
-- and lower right corner.
showBoard :: Position -> Position -> Board -> IO ()
showBoard (x1,y1) (x2,y2) board = do
    forM_ [y2..y1] $ \y -> do
        forM_ [x1..x2] $ \x -> do
            if (x,y) `elem` board 
                then putStr " * "
                else putStr "   "
        putStrLn ""


-- For each cell in the board and for each possible neighbour cell determine
-- the life status and return a new Board.
step :: Board -> Board
step board = nub (checkAlive ++ becomesAlive )
  where checkAlive   = filter (alive board) board
        becomesAlive = concatMap (filter (alive board) . neighbours) board


-- For a specific position, return its neighbours.    
neighbours :: Position -> [Position]
neighbours (x,y) = 
    [(x-1,y), (x+1,y), (x,y-1), (x,y+1)             -- adjacent
    ,(x-1,y-1), (x+1,y-1), (x+1,y+1), (x-1,y+1)]    -- diagonal


-- Check, if the position stays or becomes alive.
alive :: Board -> Position -> Bool
alive board pos =
    let ns  = neighbours pos
        num = length $ board `intersect` ns
        ali = pos `elem` board
    in (ali && (num == 2 || num == 3)) || (not ali && num == 3)
