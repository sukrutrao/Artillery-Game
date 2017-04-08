module Callback (reshape , display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Gamestate
import Rectangle
import Line
import qualified Tank
import qualified Physics

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

display :: IORef GameState -> DisplayCallback
display gamestate = do
        clear [ColorBuffer, DepthBuffer]
        game <- get gamestate

        --Drawing The Tiles
        print "Drawing Tiles"
        forM_ (tileMatrix game) $ \(tileList) -> do
            forM_ (tileList) $ \(Tile {tileposition = (Physics.Position x y),isObstacle = w }) -> do
                loadIdentity
                if (w == True)
                    then currentColor $= Color4 0 0.5019 0 1            --green obstacle
                    else currentColor $= Color4 0.6 0.8 1 1    --background color non obstacle
                translate $ Vector3 x y 0
                rectangle widthOfTile heightOfTile
                flush

        --Drawing The White power Button
        loadIdentity
        currentColor $= Color4 1 1 1 1              -- white power background
        translate $ Vector3 (-0.375) (-0.9) (0::Float)
        rectangle 0.75 0.1
        flush

        --Drawing The Tanks
        print "Drawing The Tanks"
        forM_ (tankList game) $ \(Tank.Tank { Tank.tankState = (Tank.TankState {
                                            Tank.direction = d,
                                            Tank.position = (Physics.Position x y),
                                            Tank.velocity = (Physics.Velocity 0 0),
                                            Tank.inclineAngle = incline_theta,
                                            Tank.turret = (Tank.Turret {
                                                Tank.angle = turret_theta, 
                                                Tank.power = turret_power
                                            })
                                        }),
                                        Tank.tankWeapons = w,
                                        Tank.score = s
                                    }) -> do
            loadIdentity
            currentColor $= Color4 0.5 0.5 0.1 1              -- red tank
            translate $ Vector3 x y 0
            rectangle Tank.widthOfTank Tank.heightOfTank

            --Drawing The Turret
            loadIdentity
            lineWidth $= 5
            currentColor $= Color4 0.34 0.34 0.1686 1     -- grey turret
            translate $ Vector3 (x+(Tank.widthOfTank/2)) (y+Tank.heightOfTank) 0
            rotate (turret_theta+incline_theta) $ Vector3 0 0 1 
            line Tank.baseOfTurret Tank.perpendicularOfTurret
            flush

            --Drawing The Red Power Bar
            loadIdentity
            currentColor $= Color4 1 0 0 1              -- red power background
            translate $ Vector3 (-0.375) (-0.9) (0::Float)
            rectangle (if(turret_power > 100) then (0.75) else((turret_power*0.75)/100)) 0.1
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

