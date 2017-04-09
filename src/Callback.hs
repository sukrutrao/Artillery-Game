module Callback (reshape , display  , idle , keyboardMouse) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import qualified Types
import qualified  Tank
import qualified Physics
import qualified Input
import Gamestate
import Rectangle
import Line
import Triangle

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

display :: IORef Types.GameState -> DisplayCallback
display gamestate = do
        clear [ColorBuffer, DepthBuffer]
        game <- get gamestate
        --Drawing The Tiles
        forM_ (Types.tileMatrix game) $ \(tileList) -> do
            forM_ (tileList) $ \(Types.Tile {Types.tilePosition = (Types.Position x y), Types.isObstacle = w }) -> do
                loadIdentity
                currentColor $= if(w) then Color4 0 0.5019 0 1 else Color4 0.6 0.8 1 1
                translate $ Vector3 x y 0
                rectangle widthOfTile heightOfTile
                flush
        --Drawing The White power Button
        loadIdentity
        currentColor $= Color4 1 1 1 1              -- white power background
        translate $ Vector3 (-0.375) (-0.9) (0::Float)
        rectangle 0.75 0.01
        flush


        --Drawing The Red Power Bar

        let currTankPower = Types.power(Types.turret (Types.tankState ((Types.tankList game) !! (Types.chance game))))
        loadIdentity
        currentColor $= Color4 1 0 0 1              -- red power background
        translate $ Vector3 (-0.375) (-0.9) (0::Float)
        rectangle ((currTankPower  *0.75)/100) 0.01
        flush


        --Drawing The Tanks
        forM_ (Types.tankList game) $ \(Types.Tank { Types.tankState = (Types.TankState {
                                            Types.direction = d,
                                            Types.position = (Types.Position x y),
                                            Types.velocity = (Types.Velocity 0 0),
                                            Types.inclineAngle = incline_theta,
                                            Types.turret = (Types.Turret {
                                                Types.angle = turret_theta, 
                                                Types.power = turret_power
                                            })
                                        }),
                                        Types.tankWeapons = w,
                                        Types.score = s,
                                        Types.color = tankcolor,
                                        Types.healthBarPosition = healthPos
                                    }) -> do
            loadIdentity
            currentColor $= tankcolor
            rotate incline_theta $ Vector3 0 0 1 
            translate $ Vector3 x y 0
            rectangle Tank.widthOfTank Tank.heightOfTank

            --Drawing The White Health Of Tank
            loadIdentity
            currentColor $= Color4 1 1 1 1              -- white health background
            translate $ Vector3 (Physics.getPositionX healthPos) (Physics.getPositionY healthPos) (0::Float)
            rectangle 0.4 0.05
            flush

            --Drawing The Health Of Tank
            loadIdentity
            currentColor $= tankcolor              -- tank color power
            translate $ Vector3 (Physics.getPositionX healthPos) (Physics.getPositionY healthPos) (0::Float)
            rectangle (max (0.0)  (((fromIntegral s)*0.4) / 30)) 0.05
            flush

            --Drawing The Turret
            loadIdentity
            lineWidth $= 5
            currentColor $= Color4 0.34 0.34 0.1686 1     -- grey turret
            translate $ Vector3 (x+(Tank.widthOfTank/2)) (y+Tank.heightOfTank) 0
            rotate (turret_theta+incline_theta) $ Vector3 0 0 1 
            line Tank.lengthOfTurret
            flush


            --Drawing The Current Triangle

        let curTank = Types.position (Types.tankState (((Types.tankList game) !! (Types.chance game))))
        loadIdentity
        currentColor $= Color4 0.8588 0.3019 1 1
        translate $ Vector3 ((Physics.getPositionX curTank) + (Tank.widthOfTank/2.0))  ((Physics.getPositionY curTank) + Tank.heightOfTank + 0.15) 0
        triangle Tank.edgeOfTriangle

        swapBuffers
        flush



keyboardMouse :: IORef Types.GameState -> KeyboardMouseCallback
keyboardMouse gamestate key Down _ _ = case key of
  Char '+' -> do
            gamestate $~! \x -> Tank.updateGameStateTank x Input.increasePower
            postRedisplay Nothing
  Char '-' -> do
            gamestate $~! \x -> Tank.updateGameStateTank x Input.decreasePower
            postRedisplay Nothing
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()



idle :: IdleCallback
idle = postRedisplay Nothing