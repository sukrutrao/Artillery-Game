import Physics
import Weapon
import Tank
import Gamestate

main = putStrLn "Hello World!\n"

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [ DoubleBuffered]
    _window <- createWindow "Artillery Game"
    windowSize $= Size 1280 720
    reshapeCallback $= Just reshape
    gamestate <- newIORef 3
    keyboardMouseCallback $= Just (keyboardMouse delta pos)
    displayCallback $= display angle pos
    idleCallback $= Just (idle angle delta)
    
    mainLoop