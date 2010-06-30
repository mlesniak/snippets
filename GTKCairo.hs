module Main where
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Data.IORef
import Control.Concurrent


main = do
    initGUI
    Just xml <- xmlNew "GTKCairo.glade"
    window   <- xmlGetWidget xml castToWindow "window"
    close    <- xmlGetWidget xml castToButton "button"


    -- Mutable value for animation
    ref <- newIORef 0.0

    darea    <- xmlGetWidget xml castToDrawingArea "drawingArea"
    darea `on` sizeRequest $ return (Requisition 400 400)
    (darea `on` exposeEvent) (update ref)
    
    -- Fork thread to change value
    forkIO (thread ref darea)

    onClicked close (widgetDestroy window)
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI


thread :: IORef Double -> DrawingArea -> IO ()
thread ref dr = do
    putStrLn "tick"
    threadDelay (10^5)
    modifyIORef ref (\rv -> toEnum $ (fromEnum rv + 5) `mod` 400)
    -- Repaint the widget with the new coordinate.
    widgetQueueDraw dr
    thread ref dr


update :: IORef Double -> EventM EExpose Bool
update ref = do
    win   <- eventWindow
    liftIO $ do
    xpos <- readIORef ref
    (w,h) <- drawableGetSize win
    let width  = realToFrac w
        height = realToFrac h

    renderWithDrawable win $ do
        setSourceRGB 1 1 1
        rectangle 0 0 width height
        fill

        setSourceRGB 0 0 0

        moveTo xpos 30
        lineTo (width-30) (height-30)
        stroke

    return True
