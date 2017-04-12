module Callback (reshape , display  , idle , keyboardMouse) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import System.IO.Unsafe
import qualified Types
import qualified  Tank
import qualified Physics
import qualified Input
import qualified Weapon
import Gamestate
import Rectangle
import Line
import Triangle
import Debug.Trace


reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

display :: IORef Types.GameState -> IORef Float -> DisplayCallback
display gamestate bulletRotationAngle = do
        clear [ColorBuffer, DepthBuffer]
        game <- get gamestate
        --Drawing The Tiles

        forM_ (Types.tileMatrix game) $ \(tileList) -> do
            forM_ (tileList) $ \(Types.Tile {Types.tilePosition = (Types.Position x y), Types.isObstacle = w }) -> do
                loadIdentity
                currentColor $= if(w) then Color4 0 0.5019 0 1 else Color4 0.6 0.8 1 1
                translate $ Vector3 x y 0
                rectangle Types.widthOfTile Types.heightOfTile
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
                                        }),
                                        Types.score = s,
                                        Types.color = tankcolor,
                                        Types.currentWeapon = current_weapon,
                                        Types.weaponCount = weapon_count
                                    }) -> do

            tankcount $~! (+1)
            putStr "\n*****\nX: "
            print x
            putStr "Y: "
            print y
            putStr "\nTheta : "
            print incline_theta
            putStr "\nTurret Theta : "
            print turret_theta
            let tankCoordX = Physics.getTilePosX (Types.tileMatrix game) y x
                tankCoordY = Physics.getTilePosY (Types.tileMatrix game) y x
                tankWidthInGLUT = (fromIntegral Types.widthOfTank)*Types.widthOfTile
                tankHeightInGLUT = (fromIntegral Types.heightOfTank)*Types.heightOfTile

            loadIdentity
            currentColor $= tankcolor
            translate $ Vector3 tankCoordX (tankCoordY) 0
            rotate (Physics.radianTodegree incline_theta) $ Vector3 0 0 1 
            rectangle tankWidthInGLUT tankHeightInGLUT

            let topCenterX = (tankCoordX+(Physics.hypotenuseRect*cos(incline_theta+Physics.rectHalfAngle)))
                topCenterY = (tankCoordY+(Physics.hypotenuseRect*sin(incline_theta+Physics.rectHalfAngle)))

            let perpendicularAngle = atan((-1)*(1/(tan(incline_theta))))

            let lengthOfTurret = (Types.lengthOfTurret ((Types.weapon game) !! current_weapon))

            let healthX = (topCenterX-(cos(incline_theta)*(tankWidthInGLUT/3))) - (lengthOfTurret*0.35)*cos(perpendicularAngle)
                healthY = (topCenterY-(sin(incline_theta)*(tankWidthInGLUT/3))) - (lengthOfTurret*1.25)*sin(perpendicularAngle)

            --Drawing The White Health Of Tank
            loadIdentity
            currentColor $= Color4 1 1 1 1              -- white health background
            translate $ Vector3 healthX healthY (0::Float)
            rotate (Physics.radianTodegree incline_theta) $ Vector3 0 0 1 
            rectangle (tankWidthInGLUT/1.5) 0.02
            flush

            --Drawing The Health Of Tank
            loadIdentity
            currentColor $= if (s>20) then Color4 0 0.5019 0 1 else (if (s>10) then Color4 1 0.8196 0.10196 1 else Color4 1 0 0 1 )               -- tank color power
            translate $ Vector3 healthX healthY (0::Float)
            rotate (Physics.radianTodegree incline_theta) $ Vector3 0 0 1
            rectangle (max (0.0)  (((fromIntegral s)*((tankWidthInGLUT/1.5)))/30)) 0.02
            flush

            --Drawing The Turret
            loadIdentity
            lineWidth $=  Types.turretThickness ((Types.weapon game) !! current_weapon)
            currentColor $= Types.turretColor ((Types.weapon game) !! current_weapon)     -- grey turret
            translate $ Vector3 topCenterX topCenterY 0
            rotate (Physics.radianTodegree (turret_theta+incline_theta)) $ Vector3 0 0 1 
            line lengthOfTurret
            flush

            --Drawing The  Current Triangle
            tankcountIO <- get tankcount
            if( tankcountIO == (Types.chance game))
                then do
                    loadIdentity
                    currentColor $= Color4 0.5588 0.0019 0.0988 1
                    translate $ Vector3 (topCenterX-(lengthOfTurret*0.55)*cos(perpendicularAngle)) (topCenterY-(lengthOfTurret*1.90)*sin(perpendicularAngle)) 0
                    rotate (Physics.radianTodegree incline_theta) $ Vector3 0 0 1 
                    triangle Physics.edgeOfTriangle
                    -- Drawing The Weapon If Launched
                    if checkifWeaponIsLaunched game
                        then do
                            bulletAngle <- get bulletRotationAngle
                            let currWeaponFromList = (Types.weapon game) !! current_weapon
                                weaponX = Physics.getPositionX $ Types.currentPosition $ Types.weaponPhysics currWeaponFromList
                                weaponY = Physics.getPositionY $ Types.currentPosition $ Types.weaponPhysics currWeaponFromList
                            if ((truncate weaponY>((length $ Types.tileMatrix game)-2)) || 
                                (weaponY<0) || 
                                (truncate weaponX>((length $ Types.tileMatrix game !! 0)-2)) ||
                                (weaponX<0)
                                )
                                    then return()
                                    else do 
                                        loadIdentity
                                        currentColor $= Types.bulletColor currWeaponFromList
                                        translate $ Vector3 (Physics.getTilePosX (Types.tileMatrix game) weaponY weaponX) (Physics.getTilePosY (Types.tileMatrix game) weaponY weaponX) (0::Float)
                                        rotate bulletAngle $ Vector3 0 0 1
                                        rectangle 0.02 0.02
                        else return()
                    flush
                else return()
                    
        swapBuffers
        flush


keyboardMouse :: IORef Types.GameState -> IORef Float -> KeyboardMouseCallback
keyboardMouse gamestate bulletRotationAngle key Down _ _ = do
        game <- get gamestate
        if Types.isAcceptingInput game
            then
                case key of
            Char '+' -> do
                    gamestate $~! \x -> Tank.updateGameStateTank x Input.increasePower
                    postRedisplay Nothing
            Char '-' -> do
                    gamestate $~! \x -> Tank.updateGameStateTank x Input.decreasePower
                    postRedisplay Nothing
            Char 'A'  -> do
                    gamestate $~! \x -> Tank.updateGameStateTank x Input.decreaseAngle
                    postRedisplay Nothing
            Char 'D' -> do
                    gamestate $~! \x -> Tank.updateGameStateTank x Input.increaseAngle
                    postRedisplay Nothing
            Char 'a'  -> do
                    gamestate $~! \x -> Tank.updateGameStateTank x Input.decreaseAngle
                    postRedisplay Nothing
            Char 'd' -> do
                    gamestate $~! \x -> Tank.updateGameStateTank x Input.increaseAngle
                    postRedisplay Nothing
            SpecialKey KeyLeft -> do
                    print("\n******************************************\n")
                    gamestate $~! \x -> Tank.updateGameStateTank x Input.moveLeft
                    postRedisplay Nothing
            SpecialKey KeyRight -> do
                    print("\n******************************************\n")
                    gamestate $~! \x -> Tank.updateGameStateTank x Input.moveRight
                    postRedisplay Nothing
            Char '0' -> do
                    putStrLn $ show (Physics.getAllPointsInCircle (Types.Position 0 0) 3 )
                    gamestate $~! \x -> Tank.updateGameStateTank x Input.weapon0
                    postRedisplay Nothing
            Char '1' -> do
                    gamestate $~! \x -> Tank.updateGameStateTank x Input.weapon1
                    postRedisplay Nothing
            Char '2' -> do
                    gamestate $~! \x -> Tank.updateGameStateTank x Input.weapon2
                    postRedisplay Nothing
            Char ' ' -> do
                    if(checkifSufficientWeaponsAvailable game)
                        then do
                            gamestate $~! \x -> Tank.updateGameStateLaunchWeapon x
                            bulletRotationAngle $~! (*0)
                            postRedisplay Nothing
                        else do
                            putStrLn "INSUFFICIENT WEAPON SUPPLY . CHOOSE OTHER WEAPON"
            _ -> return ()
            else
                return ()
keyboardMouse _ _ _ _ _ _ = return ()

checkifWeaponIsLaunched ::Types.GameState -> Bool
checkifWeaponIsLaunched (Types.GameState {
        Types.tankList = l,
        Types.weapon = w,
        Types.chance = c
    }) = Types.isLaunched $ Types.weaponPhysics $ (w !! (Types.currentWeapon (l !! c)))


checkifWeaponHAsImpacted ::Types.GameState -> Bool
checkifWeaponHAsImpacted (Types.GameState {
        Types.tankList = l,
        Types.weapon = w,
        Types.chance = c
    }) = Types.hasImpacted $ Types.weaponPhysics $ (w !! (Types.currentWeapon (l !! c)))

checkifSufficientWeaponsAvailable ::Types.GameState -> Bool
checkifSufficientWeaponsAvailable (Types.GameState {
        Types.tankList = l,
        Types.weapon = w,
        Types.chance = c
    }) = if ((Types.weaponCount $ l !! c) !! (Types.currentWeapon $ l !! c)) > 0 then True else False



idle ::IORef Types.GameState ->  IORef Float -> IdleCallback
idle gamestate bulletRotationAngle = do
    game <- get gamestate
    if (checkifWeaponIsLaunched game)
        then do 
            bulletRotationAngle $~! (+20)
            gamestate $~! \x -> Weapon.updateGameStateWeapon x
            postRedisplay Nothing
        else return()




    
