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
import Debug.Trace
    
getTileList  ::[(Float , Float)] -> [Picture]

getTileList listTile = [ translate (x*(Types.widthOfScreen/2)) (y*(Types.heightOfScreen/2)) $ color (light $ light $ light $ light blue) $ rectangleSolid (Types.widthOfTile*(Types.widthOfScreen/2)) (Types.heightOfTile*(Types.heightOfScreen/2)) | (x,y) <- listTile ]

getIsObstacleTilesList :: [Types.Tile] -> [(Float , Float)]
getIsObstacleTilesList tilemap = [ (Physics.getPositionX $ Types.tilePosition xs , Physics.getPositionY $ Types.tilePosition xs) |  xs <- (tilemap) , (Types.isObstacle xs) == False ]

getTankListGraphics :: Types.GameState -> [[Picture]]
getTankListGraphics game = [ [ trace ("*********************\n THETA : " ++ show incline_theta++ " " ++ show tankCoordX) (translate ((tankCoordX+(tankWidthInGLUT*0.5*cos(incline_theta))-(tankHeightInGLUT*0.5*sin(incline_theta)))*(Types.widthOfScreen/2)) ((tankCoordY+(tankWidthInGLUT*0.5*sin(incline_theta))+(tankHeightInGLUT*0.5*cos(incline_theta)))*(Types.heightOfScreen/2)) $ rotate (Physics.radianTodegree (-incline_theta)) $ color tankcolor  $ rectangleSolid (tankWidthInGLUT*(Types.widthOfScreen/2)) (tankHeightInGLUT*(Types.heightOfScreen/2))),
                               trace ("healthX : " ++ show healthY) (translate (healthX*(Types.widthOfScreen/2)) (healthY*(Types.heightOfScreen/2)) $ color white $ rotate (Physics.radianTodegree (-incline_theta)) $ rectangleSolid (tankWidthInGLUT*250/1.5) 2.5)  ,
                             --  translate ((healthX*500)+(tankWidthInGLUT*125/1.5)) ((healthY*250)+1.25) $ color (getcolor s)  $ rotate (Physics.radianTodegree incline_theta) $ rectangleSolid ((max (0.0)  ((s*((tankWidthInGLUT/1.5)))/30))*5) 0.05,
                               trace(show ((topCenterY))) (translate ((topCenterX + (lengthOfTurret*cos(incline_theta+turret_theta)/2))*(Types.widthOfScreen/2)) ((topCenterY + (lengthOfTurret*sin(incline_theta+turret_theta)/2))*(Types.heightOfScreen/2)) $ color red $ rotate (Physics.radianTodegree (-(turret_theta+incline_theta))) $ rectangleSolid (lengthOfTurret*(Types.widthOfScreen/2)) 2.5)
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
                                           
                                         ]


render :: Types.GameState -> Picture
render game = pictures ((getTileList (getIsObstacleTilesList (Physics.flattenList (Types.tileMatrix game))) ++ 
                       -- [ translate (-37.5) (-90) $ color white $ rectangleSolid 75 1 , translate (-37.5) (-90) $ color red $ rectangleSolid ((Types.power(Types.turret (Types.tankState ((Types.tankList game) !! (Types.chance game))))  *75)/100) 1 ] ++
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


handlekeys _ x = x


getcolor :: Float -> Color
getcolor s
  | s>=20 = green
  | s>=10 && s<20 = orange
  | s<10 = red



