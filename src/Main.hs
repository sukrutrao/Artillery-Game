
import Graphics.UI.GLUT
import Data.IORef
import Callback
 
main :: IO ()
main = do
	putStrLn "Hello World!\n"
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [ DoubleBuffered]
    _window <- createWindow "Hello World"

    windowSize $= Size 1280 720

    bulletFired <- newIORef 1
    angle <- newIORef 0

    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse bulletFired)
    idleCallback $= Just (idle angle)
    displayCallback $= display bulletFired
    

    mainLoop