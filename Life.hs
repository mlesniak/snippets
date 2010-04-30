-- Game of Life in Haskell. Not the most efficient version, but conceptually
-- very easy to understand.
--
-- TODO
--   * add some visualization for the console.
module Main where
import Data.List


type Position = (Integer, Integer)
type Board    = [Position] -- list of alive cells


-- Simple board for experimentation which cycles.
b1 = [(0,0), (1,0), (2,0)]


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
