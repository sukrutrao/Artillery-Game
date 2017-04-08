import Graphics.UI.GLUT
import Data.IORef
 
main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "Artillery Game"
    windowSize $= Size 1280 720
    mainLoop