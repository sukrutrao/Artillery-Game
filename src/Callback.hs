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

rectHalfAngle :: Float
rectHalfAngle = atan ((2*Tank.heightOfTank)/Tank.widthOfTank)

hypotenuseRect :: Float
hypotenuseRect = sqrt((Tank.heightOfTank^2) + ((Tank.widthOfTank/2)^2))

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
        --Drawing The White power Bar
        loadIdentity
        currentColor $= Color4 1 1 1 1              -- white power background
        translate $ Vector3 (-0.375) (-0.9) (0::Float)
        rectangle 0.75 0.01
        flush


        --Drawing The Red Power Bar

        loadIdentity
        currentColor $= Color4 1 0 0 1              -- red power background
        translate $ Vector3 (-0.375) (-0.9) (0::Float)
        rectangle ((Types.power(Types.turret (Types.tankState ((Types.tankList game) !! (Types.chance game))))  *0.75)/100) 0.01
        flush

        tankcount <- newIORef (-1)

        --Drawing The Tanks
        forM_ (Types.tankList game) $ \(Types.Tank  { Types.tankState = (Types.TankState {
                                            Types.direction = d,
                                            Types.position = (Types.Position x y),
                                            Types.velocity = (Types.Velocity 0 0),
                                            Types.inclineAngle = incline_theta,
                                            Types.turret = (Types.Turret {
                                                Types.angle = turret_theta, 
                                                Types.power = turret_power
                                            })
                                        })
                                        Types.score = s,
                                        Types.color = tankcolor,
                                        Types.currentWeapon = current_weapon,
                                        Types.weaponCount = weapon_count
                                    }) -> do

            tankcount $~! (+1)

            let tankCoordX = Physics.getTilePosX (Types.tileMatrix game) x y
                tankCoordY = Physics.getTilePosY (Types.tileMatrix game) x y

            loadIdentity
            currentColor $= tankcolor
            translate $ Vector3 tankCoordX tankCoordY 0
            rotate incline_theta $ Vector3 0 0 1 
            rectangle Tank.widthOfTank Tank.heightOfTank

            let topCenterX = (tankCoordX+(hypotenuseRect*cos((degreeToRadian incline_theta)+rectHalfAngle)))
                topCenterY = (tankCoordY+(hypotenuseRect*sin((degreeToRadian incline_theta)+rectHalfAngle)))

            let perpendicularAngle = atan((-1)*(1/(tan(degreeToRadian incline_theta))))

            let lengthOfTurret = (Types.lengthOfTurret ((Types.tankList game) !! current_weapon))

            let healthX = (topCenterX-(cos(degreeToRadian incline_theta)*(Tank.widthOfTank/3))) - (lengthOfTurret*0.35)*cos(perpendicularAngle)
                healthY = (topCenterY-(sin(degreeToRadian incline_theta)*(Tank.widthOfTank/3))) - (lengthOfTurret*1.25)*sin(perpendicularAngle)

            --Drawing The White Health Of Tank
            loadIdentity
            currentColor $= Color4 1 1 1 1              -- white health background
            translate $ Vector3 healthX healthY (0::Float)
            rotate incline_theta $ Vector3 0 0 1 
            rectangle (Tank.widthOfTank/1.5) 0.02
            flush

            --Drawing The Health Of Tank
            loadIdentity
            currentColor $= if (s>20) then Color4 0 0.5019 0 1 else (if (s>10) then Color4 1 0.8196 0.10196 1 else Color4 1 0 0 1 )               -- tank color power
            translate $ Vector3 healthX healthY (0::Float)
            rotate incline_theta $ Vector3 0 0 1 
            rectangle (max (0.0)  (((fromIntegral s)*((Tank.widthOfTank/1.5)))/30)) 0.02
            flush

            --Drawing The Turret
            loadIdentity
            lineWidth $= (Types.turretThickness ((Types.tankList game) !! current_weapon))
            currentColor $=  (Types.turretColor ((Types.tankList game) !! current_weapon))     -- grey turret
            translate $ Vector3 topCenterX topCenterY 0
            rotate (turret_theta+incline_theta) $ Vector3 0 0 1 
            line lengthOfTurret
            flush

            --Drawing The  Current Triangle
            tankcountIO <- get tankcount
            if( tankcountIO == (Types.chance game))
                then do
                    loadIdentity
                    currentColor $= Color4 0.5588 0.0019 0.0988 1
                    translate $ Vector3 (topCenterX-(lengthOfTurret*0.55)*cos(perpendicularAngle)) (topCenterY-(lengthOfTurret*1.90)*sin(perpendicularAngle)) 0
                    rotate incline_theta $ Vector3 0 0 1 
                    triangle Tank.edgeOfTriangle
                else
                    return()
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
  Char '0' -> do
            gamestate $~! \x -> Tank.updateGameStateTank x Input.weapon0
            postRedisplay Nothing
  Char '1' -> do
            gamestate $~! \x -> Tank.updateGameStateTank x Input.weapon1
            postRedisplay Nothing
  Char '2' -> do
            gamestate $~! \x -> Tank.updateGameStateTank x Input.weapon2
            postRedisplay Nothing
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()



idle :: IdleCallback
idle = print "IDLE" --postRedisplay Nothing