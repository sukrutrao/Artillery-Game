module Callback (idle, display , reshape , keyboard) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORefs
import Display

display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display angle pos = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  createTileMap tileMatrix
  createTank tankMatrix
  createBullet bulletMatrix
  createPowerBar powerMatrix
  createPoint pointMatrix
  swapBuffers


reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
 
keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse a p key Down _ _ = case key of
  (Char ' ') -> a $~! negate
  (Char '+') -> a $~! (* 2)
  (Char '-') -> a $~! (/ 2)
  (SpecialKey KeyLeft ) -> p $~! \(x,y) -> (x-0.1,y)
  (SpecialKey KeyRight) -> p $~! \(x,y) -> (x+0.1,y)
  (SpecialKey KeyUp   ) -> p $~! \(x,y) -> (x,y+0.1)
  (SpecialKey KeyDown ) -> p $~! \(x,y) -> (x,y-0.1)
  _ -> return ()

keyboardMouse _ _ _ _ _ _ = return ()



idle :: IdleCallback
idle = do
  postRedisplay Nothing
  

