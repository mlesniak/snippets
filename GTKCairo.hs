module Main where
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade


main = do
    initGUI
    Just xml <- xmlNew "GTKCairo.glade"
    window   <- xmlGetWidget xml castToWindow "window"
    close    <- xmlGetWidget xml castToButton "button"

    darea    <- xmlGetWidget xml castToDrawingArea "drawingArea"
    darea `on` sizeRequest $ return (Requisition 400 400)
    (darea `on` exposeEvent) update

    onClicked close (widgetDestroy window)

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI


update :: EventM EExpose Bool
update = do
    win   <- eventWindow
    liftIO $ do
    (w,h) <- drawableGetSize win
    let width  = realToFrac w
        height = realToFrac h

    renderWithDrawable win $ do
        setSourceRGB 1 1 1
        rectangle 0 0 width height
        fill

        setSourceRGB 0 0 0

        moveTo 30 30
        lineTo (width-30) (height-30)
        stroke

    return True
