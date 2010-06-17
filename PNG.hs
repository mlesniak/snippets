-- Example program showing how to draw into a PNG. Very useful for static
-- visualizations.
module Main where
import Graphics.Rendering.Cairo

main :: IO ()
main = do
    draw <- createImageSurface FormatRGB24 400 400
    renderWith draw $ do
        setSourceRGB 1 1 1
        rectangle 0 0 400 400
        fill

        setSourceRGB 0 0 0
        moveTo 10 10
        lineTo 100 100
        stroke

    surfaceWriteToPNG draw "out.png"

