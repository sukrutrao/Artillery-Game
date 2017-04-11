import Graphics.UI.GLUT
import System.IO.Unsafe
import Data.IORef
import Types
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
    putStr "Tile Matrix Dimensions\n\tRows : "
    print $ length (tileMatrix gameX)
    putStr "\tColumns : "
    print $ length ((tileMatrix gameX) !! 0)
    gamestate <- newIORef gameX
    reshapeCallback $= Just reshape
    displayCallback $= display gamestate
    keyboardMouseCallback $= Just (keyboardMouse gamestate)
   -- idleCallback $= Just (idle)
    mainLoop