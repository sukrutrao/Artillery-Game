import Graphics.UI.GLUT
import Data.IORef
import Bindings
 
main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "Hello World"
    windowSize $= Size 1280 720
    reshapeCallback $= Just reshape
    depthFunc $= Just Less
    angle <- newIORef 0
    delta <- newIORef 0.1
    pos <- newIORef (0, 0)
    keyboardMouseCallback $= Just (keyboardMouse delta pos)
    idleCallback $= Just (idle angle delta)
    displayCallback $= display angle pos
    mainLoop
