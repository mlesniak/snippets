-- Mandelbrotset in Haskell.
module Main where
import System.Environment
import Data.List

main :: IO ()
main = do
    -- Parse command line. No error messages!
    [width', height', filename] <- getArgs
    let width   = read width'  :: Double
        height  = read height' :: Double

        -- Map coordinates of the image to the Mandelbrot set.
        points  = [(x,y)| y <- [0..height-1], x <- [0..width-1]]
        diffX   = 3.0 / (width-1)
        diffY   = 2.0 / (height-1)
        mpoints = map (\(x,y) -> (-2 + x*diffX, -1 + y*diffY)) points
        values  = map (f 100) mpoints
    printSet (fromEnum width+1) $ values


-- Print set on the console
printSet :: Int -> [Bool] -> IO ()
printSet _ [] = return ()
printSet w ps = do
    let (line,rest) = splitAt (w-1) ps
        lineC       = map boolToChar line
    putStrLn lineC
    printSet w rest
  where boolToChar True  = '*'
        boolToChar False = ' '


-- Return true, if the point (x,y) is in the set. Tests for iter iterations.
f :: Int -> (Double, Double) -> Bool
f iter (cx,cy) = all (<4) $ take iter (map abs (iterate f' (0.0,0.0)))
  where f' (x,y)  = (x*x-y*y+cx, 2*x*y+cy)
        abs (x,y) = x*x+y*y


