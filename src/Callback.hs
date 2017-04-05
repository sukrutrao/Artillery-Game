module Callback (idle, display , reshape , keyboard) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import qualified Gamestate
import qualified Display

display :: IORef GLfloat -> IORef GLfloat -> DisplayCallback
display bulletFired angle = do
  clear [ColorBuffer]
  loadIdentity
 -- Display.createTileMap tileMatrix
 -- Display.createTank tankMatrix
  bulletRotationAngle <- get angle
  
  if bulletFired < 0
    then  Display.createBullet (4,5) bulletRotationAngle
    else  return ()

  --Display.createPowerBar powerMatrix
  --Display.createScore scoreMatrix
  swapBuffers


reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
 
keyboardMouse :: IORef GLfloat -> KeyboardMouseCallback
keyboardMouse bulletFired key Down _ _ = case key of
  (Char ' ') -> bulletFired $~! negate
  _ -> return ()

keyboardMouse _ _ _ _ _ _ = return ()


idle :: IORef GLfloat -> IdleCallback
idle angle = do
  angle $~! (+ 1)
  postRedisplay Nothing

