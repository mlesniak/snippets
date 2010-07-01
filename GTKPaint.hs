module Main where
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Glade
import Control.Monad


main = do
    initGUI
    Just xml <- xmlNew "GTKCairo.glade"
    window   <- xmlGetWidget xml castToWindow "window"
    close    <- xmlGetWidget xml castToButton "button"

    darea    <- xmlGetWidget xml castToDrawingArea "drawingArea"
    (darea `on` sizeRequest) (return (Requisition 400 400))
    (darea `on` exposeEvent) (update)

    -- Add mouse listener. 
    --
    -- It seems that Button1MotionMask events (and clicks in general) are not
    -- passed to the modifier, so we work around this (bug?) by forcing the
    -- user to type shift for drawing. Maybe I just don't understand some nifty
    -- detail of the event system of GTK. 
    --
    -- See http://www.haskell.org/gtk2hs/docs/gtk-docs-0.11.0/
    --     Graphics-UI-Gtk-Abstract-Widget.html#7
    -- for more information.
    widgetAddEvents darea [ButtonPressMask,PointerMotionMask]
    (darea `on` motionNotifyEvent) (button darea)
    
    onClicked close (widgetDestroy window)
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI


-- Called when the pointer is moved. See the above comment for the unusual
-- handling of the Shift-Key.
--
-- Minor remarks:
--   * Looses image when changing focus, e.g. to the close button, because
--     the Expose-Event is fired. 
button :: DrawingArea -> EventM EMotion Bool
button draw = do
    m <- eventModifier
    when (m == [Shift]) $ do
        w     <- eventWindow
        (x,y) <- eventCoordinates
        liftIO $ renderWithDrawable w $ do
            arc x y 3.0 0 (2*pi)
            fill
    return True


update :: EventM EExpose Bool
update = do
    win   <- eventWindow
    liftIO $ do
    putStrLn "Update"
    (w,h) <- drawableGetSize win
    let width  = realToFrac w
        height = realToFrac h

    renderWithDrawable win $ do
        setSourceRGB 1 1 1
        rectangle 0 0 width height
        fill

    return True
