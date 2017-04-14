module Render where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.IO.Unsafe
import Control.Monad
import qualified Types
import Gamestate
import qualified Physics
import qualified Tank
import qualified Input
import qualified Common
import qualified Weapon
import Debug.Trace
    
getTileList  ::[(Float , Float)] -> [Picture]

getTileList listTile = [ translate (x*(Types.widthOfScreen/2)) (y*(Types.heightOfScreen/2)) $ color (light $ light $ light $ light blue) $ rectangleSolid (Types.widthOfTile*(Types.widthOfScreen/2)) (Types.heightOfTile*(Types.heightOfScreen/2)) | (x,y) <- listTile ]

getIsObstacleTilesList :: [Types.Tile] -> [(Float , Float)]
getIsObstacleTilesList tilemap = [ (Physics.getPositionX $ Types.tilePosition xs , Physics.getPositionY $ Types.tilePosition xs) |  xs <- (tilemap) , (Types.isObstacle xs) == False ]

getTankListGraphics :: Types.GameState -> [[Picture]]

getTankListGraphics game = [ [ trace ("*********************\n THETA : " ++ show incline_theta++ " " ++ show tankCoordX) (translate ((tankCoordX+(tankWidthInGLUT*0.5*cos(incline_theta))-(tankHeightInGLUT*0.5*sin(incline_theta)))*(Types.widthOfScreen/2)) ((tankCoordY+(tankWidthInGLUT*0.5*sin(incline_theta))+(tankHeightInGLUT*0.5*cos(incline_theta)))*(Types.heightOfScreen/2)) $ rotate (Physics.radianTodegree (-incline_theta)) $ color tankcolor  $ unsafePerformIO (loadBMP "bodys.bmp")), -- (tankWidthInGLUT*(Types.widthOfScreen/2)) (tankHeightInGLUT*(Types.heightOfScreen/2))),
                               trace ("healthX : " ++ show healthY) (translate (healthX*(Types.widthOfScreen/2)) (healthY*(Types.heightOfScreen/2)) $ color white $ rotate (Physics.radianTodegree (-incline_theta)) $ rectangleSolid (tankWidthInGLUT*250/1.5) 2.5)  ,
                             --  translate ((healthX*500)+(tankWidthInGLUT*125/1.5)) ((healthY*250)+1.25) $ color (getcolor s)  $ rotate (Physics.radianTodegree incline_theta) $ rectangleSolid ((max (0.0)  ((s*((tankWidthInGLUT/1.5)))/30))*5) 0.05,
                               trace(show ((topCenterY))) (translate ((topCenterX + (lengthOfTurret*cos(incline_theta+turret_theta)/2))*(Types.widthOfScreen/2)) ((topCenterY + (lengthOfTurret*sin(incline_theta+turret_theta)/2))*(Types.heightOfScreen/2)) $ color red $ rotate (Physics.radianTodegree (-(turret_theta+incline_theta))) $ unsafePerformIO (loadBMP "turret.bmp")) ,--(lengthOfTurret*(Types.widthOfScreen/2)) 2.5)
                               if checkifWeaponIsLaunched game
                                  then translate ((Physics.getTilePosX (Types.tileMatrix game) weaponY weaponX)*(Types.widthOfScreen/2)) ((Physics.getTilePosY (Types.tileMatrix game) weaponY weaponX)*(Types.heightOfScreen/2)) $ color (Types.bulletColor currWeaponFromList) $ rectangleSolid (0.02*(Types.widthOfScreen/2)) (0.02*(Types.heightOfScreen/2))
                                  else translate 0 0 $ rectangleSolid (0.02) (0.02)
                             ] |        (Types.Tank  { Types.tankState = (Types.TankState {
                                            Types.direction = d,
                                            Types.position = (Types.Position x y),
                                            Types.velocity = (Types.Velocity _ _),
                                            Types.inclineAngle = incline_theta,
                                            Types.turret = (Types.Turret {
                                                Types.angle = turret_theta, 
                                                Types.power = turret_power
                                                    })
                                            }),
                                            Types.score = s,
                                            Types.tankcolor = tankcolor,
                                            Types.currentWeapon = current_weapon,
                                            Types.weaponCount = weapon_count
                                        }) <- (Types.tankList game) , 
                                        let tankCoordX = (Physics.getTilePosX (Types.tileMatrix game) y x)
                                            tankCoordY = (Physics.getTilePosY (Types.tileMatrix game) y x)
                                            tankWidthInGLUT = ((fromIntegral Types.widthOfTank)*Types.widthOfTile)
                                            tankHeightInGLUT = (fromIntegral Types.heightOfTank)*Types.heightOfTile
                                            topCenterX = (tankCoordX-tankHeightInGLUT*sin(incline_theta)+(tankWidthInGLUT*cos(incline_theta))/2)
                                            topCenterY = (tankCoordY+tankHeightInGLUT*cos(incline_theta)+(tankWidthInGLUT*sin(incline_theta))/2)
                                            taninverse = atan((-1)*(1/(tan(fromIntegral $ truncate incline_theta))))
                                            perpendicularAngle =  if (taninverse > 0) then taninverse else ((pi)-taninverse)
                                            lengthOfTurret = 0.1
                                            healthY = (topCenterY+(lengthOfTurret+0.01)*cos(incline_theta))
                                            healthX = (topCenterX-(lengthOfTurret+0.01)*sin(incline_theta))
                                            currWeaponFromList =  (Types.weapon game) !! current_weapon
                                            weaponX = Physics.getPositionX $ Types.currentPosition $ Types.weaponPhysics currWeaponFromList
                                            weaponY = Physics.getPositionY $ Types.currentPosition $ Types.weaponPhysics currWeaponFromList
                                         ]

render :: Types.GameState -> Picture

render game = pictures ( [( scale (1000/1200) (500/600) $ unsafePerformIO (loadBMP "map.bmp"))] ++(getTileList (getIsObstacleTilesList (Physics.flattenList (Types.tileMatrix game))) ++ 
                         [ translate 0 (-150)  $ color white $ rectangleSolid 275 2.5 , translate (-137.5 + (Types.power(Types.turret (Types.tankState ((Types.tankList game) !! (Types.chance game)))))*1.375) (-150)  $ color red $ rectangleSolid ((Types.power(Types.turret (Types.tankState ((Types.tankList game) !! (Types.chance game))))*275)/100) 2.5 ] ++

                          (Physics.flattenList(getTankListGraphics game))
                        ))
                    


handlekeys :: Event -> Types.GameState -> Types.GameState
handlekeys (EventKey (Char '+') Down _ _) game = Tank.updateGameStateTank game Input.increasePower
handlekeys (EventKey (Char '-') Down _ _) game = Tank.updateGameStateTank game Input.decreasePower

handlekeys (EventKey (Char 'A') Down _ _) game = Tank.updateGameStateTank game Input.increaseAngle
handlekeys (EventKey (Char 'D') Down _ _) game = Tank.updateGameStateTank game Input.decreaseAngle
handlekeys (EventKey (Char 'a') Down _ _) game = Tank.updateGameStateTank game Input.increaseAngle
handlekeys (EventKey (Char 'd') Down _ _) game = Tank.updateGameStateTank game Input.decreaseAngle

handlekeys (EventKey (SpecialKey KeyRight) Down _ _) game = Tank.updateGameStateTank (Tank.updateTankGravity game) Input.moveRight
handlekeys (EventKey (SpecialKey KeyLeft) Down _ _) game = Tank.updateGameStateTank (Tank.updateTankGravity game) Input.moveLeft

handlekeys (EventKey (Char '0') Down _ _) game = Tank.updateGameStateTank game Input.weapon0
handlekeys (EventKey (Char '1') Down _ _) game = Tank.updateGameStateTank game Input.weapon1
handlekeys (EventKey (Char '2') Down _ _) game = Tank.updateGameStateTank game Input.weapon2

handlekeys (EventKey (Char 's') Down _ _) game = if(checkifSufficientWeaponsAvailable game) 
                                                    then trace("IN SPACE : ") (Tank.updateGameStateLaunchWeapon game)
                                                    else trace("IN FALSE SPACE : ")  (game)
handlekeys _ x = x


update :: Float -> Types.GameState -> Types.GameState
update _ game = if (checkifWeaponIsLaunched game)
                then (Weapon.updateGameStateWeapon game)
                else trace ("IN FALSE UPDATE : ") (game)

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




getcolor :: Float -> Color
getcolor s
  | s>=20 = green
  | s>=10 && s<20 = orange
  | s<10 = red



