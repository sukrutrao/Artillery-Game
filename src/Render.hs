module Render where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.IO.Unsafe
import Types
import GameState

render :: GameState -> Picture
render game = do
    let tankCoordX =  {-trace ("INSIDE COORDX")-} (Physics.getTilePosX (Types.tileMatrix game) y x)
                tankCoordY =  {-trace ("INSIDE COORDY")-} (Physics.getTilePosY (Types.tileMatrix game) y x)
                tankWidthInGLUT = (fromIntegral Types.widthOfTank)*Types.widthOfTile
                tankHeightInGLUT = (fromIntegral Types.heightOfTank)*Types.heightOfTile
    let topCenterX = (tankCoordX+(Physics.hypotenuseRect*cos(incline_theta+Physics.rectHalfAngle)))
                topCenterY = (tankCoordY+(Physics.hypotenuseRect*sin(incline_theta+Physics.rectHalfAngle)))
            let taninverse = atan((-1)*(1/(tan(fromIntegral $ truncate incline_theta))))
            let perpendicularAngle =  if (taninverse > 0) 
                                        then taninverse
                                        else ((pi)-taninverse)

            let lengthOfTurret = (Types.lengthOfTurret ((Types.weapon game) !! current_weapon))
            

            let healthX =  (topCenterX-(cos(incline_theta)*(tankWidthInGLUT/3))) - (lengthOfTurret*0.35)*cos(perpendicularAngle)
                healthY = (topCenterY-(sin(incline_theta)*(tankWidthInGLUT/3))) - (lengthOfTurret*1.25)*sin(perpendicularAngle)

    pictures
    [ -- Drawing Tiles
    forM_ (Types.tileMatrix game) $ \(tileList) -> do
            forM_ (tileList) $ \(Types.Tile {Types.tilePosition = (Types.Position x y), Types.isObstacle = w }) -> do
                translate x y $ color if(w) then green else (light (light blue)) $ rectangleSolid Types.widthOfTile Types.heightOfTile
    , -- Drawing white power bar
    translate (-37.5) (-90) $ color white $ rectangleSolid 75 1
    , -- Drawing red power bar
    translate (-37.5) (-90) $ color red $ rectangleSolid ((Types.power(Types.turret (Types.tankState ((Types.tankList game) !! (Types.chance game))))  *75)/100) 1
    , -- Drawing the Tanks
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
                                        Types.colort = tankcolor,
                                        Types.currentWeapon = current_weapon,
                                        Types.weaponCount = weapon_count
                                    }) -> do


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

            translate tankCoordX tankCoordY $ rotate (Physics.radianTodegree incline_theta) $ color colort $ rectangleSolid tankWidthInGLUT tankHeightInGLUT
            
            , --Drawing The White Health Of Tank
            translate healthX healthY $ rotate (Physics.radianTodegree incline_theta) $ color white $ rectangleSolid ((tankWidthInGLUT/1.5)*100) 2

            , --Drawing The Health Of Tank
            translate healthX healthY $ rotate (Physics.radianTodegree incline_theta) $ color getcolor $ rectangleSolid ((max (0.0)  ((s*((tankWidthInGLUT/1.5)))/30))*100 ) 2

            ] --Drawing the turret










getcolor :: Float -> Color
getcolor s
  | s>=20 = green
  | s>=10 && s<20 = orange
  | s<10 = red







