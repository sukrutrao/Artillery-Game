module Callback (reshape , display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Gamestate
import Rectangle
import qualified Physics

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

display :: IORef GameState -> DisplayCallback
display gamestate = do
        clearColor $= Color4 0.5 0.5 0.1 1        --background color
        clear [ColorBuffer, DepthBuffer]
        loadIdentity
        game <- get gamestate
        print "Drawing Tiles"
        forM_ (tileMatrix game) $ \(tileList) -> do
            forM_ (tileList) $ \(Tile {tileposition = (Physics.Position x y),isObstacle = w }) -> do
                loadIdentity
                if (w == True)
                    then currentColor $= Color4 0 1 0 1            --green obstacle
                    else currentColor $= Color4 0.5 0.5 0.1 1    --background color non obstacle
                translate $ Vector3 x y 0
                rectangle widthOfTile heightOfTile
                flush
        swapBuffers
        flush
 

{-

keyboardMouse :: IORef Float -> IORef (Float, Float) -> KeyboardMouseCallback
keyboardMouse a p key Down _ _ = case key of
  (Char ' ') -> modifyIORef a (negate)
  (Char '+') -> modifyIORef a (*2)
  (Char '-') -> modifyIORef a (/2)
  (SpecialKey KeyLeft ) -> p $~! \(x,y) -> (x-0.1,y)
  (SpecialKey KeyRight) -> p $~! \(x,y) -> (x+0.1,y)
  (SpecialKey KeyUp   ) -> p $~! \(x,y) -> (x,y+0.1)
  (SpecialKey KeyDown ) -> p $~! \(x,y) -> (x,y-0.1)
  _ -> return ()
  
keyboardMouse _ _ _ _ _ _ = return ()


 -}

