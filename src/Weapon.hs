module Weapon where

import qualified Graphics.UI.GLUT
import Types
import Physics
import Debug.Trace

defaultStartVelocity :: Float
defaultStartVelocity = 1

genericImpactRadius :: Float
genericImpactRadius = 10

initializeWeapon posX posY factor currAngle radius bullColor turrColor bullRotate turrThick turrLen =  
                                    WeaponGraphics { weaponPhysics = (GenericWeapon { currentPosition = (originPosition posX posY),
                                                                                      currentVelocity = defaultStartVelocity,
                                                                                      launchVelocity = 0.0,
                                                                                      launchAngle = 0.0,
                                                                                      velocityMultiplyingFactor = factor,
                                                                                      currentAngle = currAngle,
                                                                                      impactRadius = radius,
                                                                                      isLaunched = False,
                                                                                      hasImpacted = False,
                                                                                      launchDirection = FacingRight
                                                                                    }
                                                                     ),
                                                    bulletColor = bullColor,
                                                    turretColor = turrColor,
                                                    bulletRotation = bullRotate,
                                                    turretThickness = turrThick,
                                                    lengthOfTurret = turrLen
                                                   }


updatePositionWeapon :: WeaponGraphics -> GameState -> [[Tile]] -> WeaponGraphics
updatePositionWeapon     (WeaponGraphics {
        weaponPhysics = (GenericWeapon {
            currentPosition = (Position x y),
            currentVelocity = velocity,
            launchVelocity = lvelocity,
            launchAngle = lAngle,
            velocityMultiplyingFactor = f,
            currentAngle = theta, 
            impactRadius = r,
            isLaunched = islaunched,
            hasImpacted = hasimpacted,
            launchDirection = l
        }),
        bulletColor = bColor,
        turretColor = tColor,
        bulletRotation = bRotate,
        turretThickness = tTurr,
        lengthOfTurret = lTurr
    }) gameState tileMap = if islaunched then 
                                if (truncate y>((length tileMap)-2) || y<0) then (WeaponGraphics {
                                    weaponPhysics = (GenericWeapon {
                                        currentPosition = {-trace("if if x : " ++ show x ++ " y : " ++ show y ++ "\n") -}{-getPositionProjectile (Position x y) velocity theta-}
                                             newPositionProjectile gameState (getTurretPosition gameState lTurr) (Position x y) velocity theta lvelocity lAngle tileMap,
                                        currentVelocity = getVelocityProjectile velocity theta,
                                        launchVelocity = lvelocity,
                                        launchAngle = lAngle,
                                        velocityMultiplyingFactor = f,
                                        currentAngle = getAngleProjectile velocity theta l,
                                        impactRadius = r,
                                        isLaunched = islaunched,
                                        hasImpacted = hasimpacted,
                                        launchDirection = l
                                    }),
                                    bulletColor = bColor,
                                    turretColor = tColor,
                                    bulletRotation = bRotate,
                                    turretThickness = tTurr,
                                    lengthOfTurret = lTurr
                                    })
                                else if (truncate x>((length $ tileMap !! 0)-2) || x<0) then (WeaponGraphics {
                                    weaponPhysics = (GenericWeapon {
                                        currentPosition = {-trace("HOLA$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" ++ show x ++ "  ,  " ++ show y ++ "\n")-} (Position x y),
                                        currentVelocity = velocity,
                                        velocityMultiplyingFactor = f,
                                        launchVelocity = lvelocity,
                                        launchAngle = lAngle,
                                        currentAngle = theta, 
                                        impactRadius = r,
                                        isLaunched = False,
                                        hasImpacted = True,
                                        launchDirection = l
                                    }),
                                    bulletColor = bColor,
                                    turretColor = tColor,
                                    bulletRotation = bRotate,
                                    turretThickness = tTurr,
                                    lengthOfTurret = lTurr
                                    })
                                    else if getIsObstacle tileMap y x || checkAllTanksForHit gameState (Position x y)
                                        then (WeaponGraphics {
                                            weaponPhysics = (GenericWeapon {
                                                currentPosition = trace("if else if x : " ++ show x ++ " y : " ++ show y ++ "\n") (Position x y),
                                                currentVelocity = velocity,
                                                velocityMultiplyingFactor = f,
                                                launchVelocity = lvelocity,
                                                launchAngle = lAngle,
                                                currentAngle = theta, 
                                                impactRadius = r,
                                                isLaunched = False,
                                                hasImpacted = True,
                                                launchDirection = l
                                            }),
                                            bulletColor = bColor,
                                            turretColor = tColor,
                                            bulletRotation = bRotate,
                                            turretThickness = tTurr,
                                            lengthOfTurret = lTurr
                                            })
                                    else (WeaponGraphics {
                                    weaponPhysics = (GenericWeapon {
                                        currentPosition = trace(" : " ++ show x ++ " y : " ++ show y ++ "\n") {-getPositionProjectile (Position x y) velocity theta-}
                                                newPositionProjectile gameState (getTurretPosition gameState lTurr) (Position x y) velocity theta lvelocity lAngle tileMap,
                                        currentVelocity = getVelocityProjectile velocity theta,
                                        velocityMultiplyingFactor = f,
                                        launchVelocity = lvelocity,
                                        launchAngle = lAngle,
                                        currentAngle = getAngleProjectile velocity theta l,
                                        impactRadius = r,
                                        isLaunched = islaunched,
                                        hasImpacted = hasimpacted,
                                        launchDirection = l

                                    }),
                                    bulletColor = bColor,
                                    turretColor = tColor,
                                    bulletRotation = bRotate,
                                    turretThickness = tTurr,
                                    lengthOfTurret = lTurr
                                    })
                 else (WeaponGraphics {
                    weaponPhysics = (GenericWeapon {
                        currentPosition = {-trace("else x : " ++ show x ++ " y : " ++ show y ++ "\n") -}(Position x y),
                        currentVelocity = velocity,
                        launchVelocity = lvelocity,
                        launchAngle = lAngle,
                        velocityMultiplyingFactor = f,
                        currentAngle = theta, 
                        impactRadius = r,
                        isLaunched = islaunched,
                        hasImpacted = hasimpacted,
                        launchDirection = l
                    }),
                    bulletColor = bColor,
                    turretColor = tColor,
                    bulletRotation = bRotate,
                    turretThickness = tTurr,
                    lengthOfTurret = lTurr
                    })

updateWeapon :: WeaponGraphics -> GameState -> [[Tile]] -> WeaponGraphics 
updateWeapon weapon gameState tileMap = if hasImpacted (weaponPhysics weapon)
                                then weapon
                                else updatePositionWeapon weapon gameState tileMap 


updateHealth :: Weapon -> Tank -> Tank
updateHealth  (GenericWeapon {
        currentPosition = (Position wx wy),
        currentVelocity = weapon_velocity,
        velocityMultiplyingFactor = vf,
        currentAngle = weapon_theta, 
        impactRadius = impactradius,
        isLaunched = isL,
        hasImpacted = hasImp,
        launchDirection = lD
    }) (Tank {
        tankState = (TankState {
            direction = d,
            position = (Position x y),
            velocity = (Velocity vx vy),
            inclineAngle = incline_theta,
            turret = (Turret {
                angle = turret_theta, 
                power = turret_power
            })
        }),
        score = s,
        tankcolor = c,
        currentWeapon = e,
        weaponCount = f
    }) =  (Tank {
        tankState = (TankState {
            direction = d,
            position = (Position x y),
            velocity = (Velocity vx vy),
            inclineAngle = incline_theta,
            turret = (Turret {
                angle = turret_theta, 
                power = turret_power
            })
        }),
        score = trace("Old score : " ++ show s ++ " New score : " ++ show (convertPointListToInteger $ getAllPointsInCircle (Position wx wy) impactradius) ++ " " ++ show (convertPointListToInteger $ getAllPointsInRectangle (Position x y) widthOfTank heightOfTank incline_theta))
            (s - (fromIntegral (length $ commonPointsBetweenLists (convertPointListToInteger $  getAllPointsInCircle (Position wx wy) impactradius) (convertPointListToInteger $ getAllPointsInRectangle (Position x y) widthOfTank heightOfTank incline_theta)) * weapon_velocity * 0.001)),
        tankcolor = c,
        currentWeapon = e,
        weaponCount = f
    })



updateGameStateWeapon :: GameState -> GameState
updateGameStateWeapon
    (GameState {
        tileMatrix = t,
        tankList = l,
        weapon = w,
        chance = c,
        noOfPlayers = n,
        isAcceptingInput = d
    }) = let weaponChoice = currentWeapon (l !! c)
             firedWeapon = w !! weaponChoice
             newWeapon = updateWeapon firedWeapon (GameState {
                                                    tileMatrix = t,
                                                    tankList = l,
                                                    weapon = w,
                                                    chance = c,
                                                    noOfPlayers = n,
                                                    isAcceptingInput = d
                                                }) t
             shouldWeBlast =  if(isLaunched $ weaponPhysics newWeapon) then t
                                else makeTileNotObsAtPts t (getAllPointsInCircle (currentPosition (weaponPhysics newWeapon)) (impactRadius (weaponPhysics newWeapon)))
             newWeaponList = changeListElementAtIndex w weaponChoice newWeapon
         in GameState { tileMatrix = shouldWeBlast, 
                        tankList =  map (updateHealth (weaponPhysics newWeapon)) l,
                        weapon = newWeaponList,
                        chance = if (isLaunched $ weaponPhysics newWeapon) then c else ((c+1) `mod` 2) ,
                        noOfPlayers = n,
                        isAcceptingInput = if (isLaunched $ weaponPhysics newWeapon) then False else True
                      }
