-- Mandelbrotset in Haskell.
--
-- TODO
--   * error message for command line parameters
module Main where
import System.Environment
import Text.Printf
import Data.List


-- A PPM file describes the filename and dimensions of an image. Since it is
-- monochrome, it stores the values of the pixels as booleans.
data PPM = PPMFile {
      ppmName   :: String
    , ppmWidth  :: Int
    , ppmHeight :: Int
    , ppmData   :: [Bool] -- Should be abstracted later to a generalized
                          -- image type.
} 

main :: IO ()
main = do
    -- Parse command line. No error messages!
    [width', height', filename] <- getArgs
    let width   = read width'  :: Double
        height  = read height' :: Double

    -- Map coordinates of the image to the Mandelbrot set.
    let points  = [(x,y)| y <- [0..height-1], x <- [0..width-1]]
        diffX   = 3.0 / (width-1)
        diffY   = 2.0 / (height-1)
        mpoints = map (\(x,y) -> (-2 + x*diffX, -1 + y*diffY)) points
        values  = map (f 100) mpoints
        ppm     = PPMFile filename (fromEnum width) (fromEnum height) values

    writePPMFile ppm


-- Writes a PPMFile.
writePPMFile :: PPM -> IO ()
writePPMFile (PPMFile name w h img) = do
   let header = printf "P1\n%d %d\n1\n" w h
       imgd   = intercalate " " $ map boolToNum img
   writeFile name (header ++ imgd)
 where boolToNum True  = "1"
       boolToNum False = "0"


-- Return true, if the point (x,y) is in the set. Tests for iter iterations.
f :: Int -> (Double, Double) -> Bool
f iter (cx,cy) = all (<4) $ take iter (map abs (iterate f' (0.0,0.0)))
  where f' (x,y)  = (x*x-y*y+cx, 2*x*y+cy)
        abs (x,y) = x*x+y*y


