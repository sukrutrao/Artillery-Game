module Render where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.IO.Unsafe
import Control.Monad
import qualified Types
import Gamestate
import qualified Physics
import Debug.Trace
    
getTileList  ::[(Float , Float)] -> [Picture]

getTileList listTile = [ translate (x*500) (y*250) $ color (light $ light $ light $ light blue) $ rectangleSolid (Types.widthOfTile*500) (Types.heightOfTile*250) | (x,y) <- listTile ]

getIsObstacleTilesList :: [[Types.Tile]] -> [(Float , Float)]
getIsObstacleTilesList tilemap = [ (Physics.getPositionX $ Types.tilePosition xs , Physics.getPositionY $ Types.tilePosition xs) |  xs <- (Physics.flattenList tilemap) , (Types.isObstacle xs) == False ]

getTankListGraphics :: Types.GameState -> [[Picture]]
getTankListGraphics game = [ [ translate ((tankCoordX*500)+(tankWidthInGLUT*125)) ((tankCoordY*250)+(tankHeightInGLUT*62.5)) $ color tankcolor $ rotate (Physics.radianTodegree incline_theta) $ rectangleSolid (tankWidthInGLUT*250) (tankHeightInGLUT*125),
                               trace(show healthY) (translate ((healthX*500)+(tankWidthInGLUT*125/1.5)) ((healthY*250)+1.25) $ color white $ rotate (Physics.radianTodegree incline_theta) $ rectangleSolid (tankWidthInGLUT*250/1.5) 2.5)  ,
                               translate ((healthX*500)+(tankWidthInGLUT*125/1.5)) ((healthY*250)+1.25) $ color (getcolor s)  $ rotate (Physics.radianTodegree incline_theta) $ rectangleSolid ((max (0.0)  ((s*((tankWidthInGLUT/1.5)))/30))*5) 0.05, 
                               translate ((topCenterX*500)+(lengthOfTurret*5*2.5)) ((topCenterY*250)-(0.25*1.25)) $ color (Types.turretColor ((Types.weapon game) !! current_weapon)) $ rotate (Physics.radianTodegree (turret_theta+incline_theta)) $ rectangleSolid (lengthOfTurret*5) 0.25
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
                                            tankWidthInGLUT = trace ("tankCoordX" ++ show tankCoordX ++ "tankCoordY" ++ show tankCoordY ) ((fromIntegral Types.widthOfTank)*Types.widthOfTile)
                                            tankHeightInGLUT = (fromIntegral Types.heightOfTank)*Types.heightOfTile
                                            topCenterX = (tankCoordX+(Physics.hypotenuseRect*cos(incline_theta+Physics.rectHalfAngle)))
                                            topCenterY = (tankCoordY+(Physics.hypotenuseRect*sin(incline_theta+Physics.rectHalfAngle)))
                                            taninverse = atan((-1)*(1/(tan(fromIntegral $ truncate incline_theta))))
                                            perpendicularAngle =  if (taninverse > 0) then taninverse else ((pi)-taninverse)
                                            lengthOfTurret = (Types.lengthOfTurret ((Types.weapon game) !! current_weapon))
                                            healthY = (topCenterY-(sin(incline_theta)*(tankWidthInGLUT/3))) - (lengthOfTurret*1.25)*sin(perpendicularAngle)
                                            healthX = ((topCenterX-(cos(incline_theta)*(tankWidthInGLUT/3))) - (lengthOfTurret*0.35)*cos(perpendicularAngle))
                                         ]


render :: Types.GameState -> Picture
render game = pictures ((getTileList (getIsObstacleTilesList (Types.tileMatrix game))) ++ 
                        [ translate (-37.5) (-90) $ color white $ rectangleSolid 75 1 , translate (-37.5) (-90) $ color red $ rectangleSolid ((Types.power(Types.turret (Types.tankState ((Types.tankList game) !! (Types.chance game))))  *75)/100) 1 ] ++
                          (Physics.flattenList(getTankListGraphics game))
                        )
                        






    {-let tankCoordX =  {-trace ("INSIDE COORDX")-} (Physics.getTilePosX (Types.tileMatrix game) y x)
        tankCoordY =  {-trace ("INSIDE COORDY")-} (Physics.getTilePosY (Types.tileMatrix game) y x)
        tankWidthInGLUT = (fromIntegral Types.widthOfTank)*Types.widthOfTile
        tankHeightInGLUT = (fromIntegral Types.heightOfTank)*Types.heightOfTile
    let topCenterX = (tankCoordX+(Physics.hypotenuseRect*cos(incline_theta+Physics.rectHalfAngle)))
        topCenterY = (tankCoordY+(Physics.hypotenuseRect*sin(incline_theta+Physics.rectHalfAngle)))
        taninverse = atan((-1)*(1/(tan(fromIntegral $ truncate incline_theta))))
        perpendicularAngle =  if (taninverse > 0) then taninverse else ((pi)-taninverse)
        lengthOfTurret = (Types.lengthOfTurret ((Types.weapon game) !! current_weapon))
        healthX =  (topCenterX-(cos(incline_theta)*(tankWidthInGLUT/3))) - (lengthOfTurret*0.35)*cos(perpendicularAngle)
        healthY = (topCenterY-(sin(incline_theta)*(tankWidthInGLUT/3))) - (lengthOfTurret*1.25)*sin(perpendicularAngle)
    -}

    {-  forM_ (Types.tankList game) $ \(Types.Tank  { Types.tankState = (Types.TankState {
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
                                        Types.tankcolor = tankcolor,
                                        Types.currentWeapon = current_weapon,
                                        Types.weaponCount = weapon_count
                                    }) -> do
-}

            {-putStr "\n*****\nX: "
            print x
            putStr "Y: "
            print y
            putStr "Theta : "
            print incline_theta
            putStr "Turret Theta : "
            print turret_theta
            putStr "TankCoordX : "
            print tankCoordX
            putStr "tankCoordY : "
            print tankCoordY-}

           -- translate tankCoordX tankCoordY $ rotate (Physics.radianTodegree incline_theta) $ color colort $ rectangleSolid tankWidthInGLUT tankHeightInGLUT
            
            --, --Drawing The White Health Of Tank
            --translate healthX healthY $ rotate (Physics.radianTodegree incline_theta) $ color white $ rectangleSolid ((tankWidthInGLUT/1.5)*100) 2

            --, --Drawing The Health Of Tank
            --translate healthX healthY $ rotate (Physics.radianTodegree incline_theta) $ color getcolor $ rectangleSolid ((max (0.0)  ((s*((tankWidthInGLUT/1.5)))/30))*100 ) 2

           -- ] --Drawing the turret










getcolor :: Float -> Color
getcolor s
  | s>=20 = green
  | s>=10 && s<20 = orange
  | s<10 = red







