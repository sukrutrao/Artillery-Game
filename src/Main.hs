import Graphics.UI.GLUT
import System.IO.Unsafe
import Data.IORef
import Gamestate
import Callback

main :: IO ()
main = do
    putStrLn "Hello World!\n"
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    _window <- createWindow "Artillery Game"
    windowSize $= Size 1280 720
    let gameX = initializeGamestate
    gamestate <- newIORef gameX
    reshapeCallback $= Just reshape
    displayCallback $= display gamestate
    keyboardMouseCallback $= Just (keyboardMouse gamestate)
    mainLoop