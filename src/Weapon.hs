module Weapon where

import qualified Graphics.UI.GLUT
import Types
import Physics
import Debug.Trace

defaultStartVelocity :: Float
defaultStartVelocity = 1

genericImpactRadius :: Float
genericImpactRadius = 10

initializeWeapon :: Float -> Float -> Float -> Float -> Float -> Graphics.UI.GLUT.Color4 Float -> Graphics.UI.GLUT.Color4 Float ->  Graphics.UI.GLUT.Vector3 Float -> Float -> Float -> WeaponGraphics
initializeWeapon posX posY factor currAngle radius bullColor turrColor bullRotate turrThick turrLen =  
                                    WeaponGraphics { weaponPhysics = (GenericWeapon { currentPosition = (originPosition posX posY),
                                                                                      currentVelocity = defaultStartVelocity,
                                                                                      velocityMultiplyingFactor = factor,
                                                                                      currentAngle = currAngle,
                                                                                      impactRadius = radius,
                                                                                      isLaunched = False,
                                                                                      hasImpacted = False
                                                                                    }
                                                                     ),
                                                    bulletColor = bullColor,
                                                    turretColor = turrColor,
                                                    bulletRotation = bullRotate,
                                                    turretThickness = turrThick,
                                                    lengthOfTurret = turrLen
                                                   }


updatePositionWeapon :: WeaponGraphics -> [[Tile]] -> WeaponGraphics
updatePositionWeapon     (WeaponGraphics {
        weaponPhysics = (GenericWeapon {
            currentPosition = (Position x y),
            currentVelocity = velocity,
            velocityMultiplyingFactor = f,
            currentAngle = theta, 
            impactRadius = r,
            isLaunched = islaunched,
            hasImpacted = hasimpacted
        }),
        bulletColor = bColor,
        turretColor = tColor,
        bulletRotation = bRotate,
        turretThickness = tTurr,
        lengthOfTurret = lTurr
    }) tileMap = if islaunched then 
                                if (truncate y>((length tileMap)-2) || y<0 || truncate x>((length $ tileMap !! 0)-2) || x<0) then (WeaponGraphics {
                                    weaponPhysics = (GenericWeapon {
                                        currentPosition = trace("if if x : " ++ show x ++ " y : " ++ show y ++ "\n") getPositionProjectile (Position x y) velocity theta,
                                        currentVelocity = getVelocityProjectile velocity theta,
                                        velocityMultiplyingFactor = f,
                                        currentAngle = getAngleProjectile velocity theta, 
                                        impactRadius = r,
                                        isLaunched = islaunched,
                                        hasImpacted = hasimpacted
                                    }),
                                    bulletColor = bColor,
                                    turretColor = tColor,
                                    bulletRotation = bRotate,
                                    turretThickness = tTurr,
                                    lengthOfTurret = lTurr
                                    })
                                else if getIsObstacle tileMap y x then (WeaponGraphics {
                                    weaponPhysics = (GenericWeapon {
                                        currentPosition = trace("if else if x : " ++ show x ++ " y : " ++ show y ++ "\n") (Position x y),
                                        currentVelocity = velocity,
                                        velocityMultiplyingFactor = f,
                                        currentAngle = theta, 
                                        impactRadius = r,
                                        isLaunched = False,
                                        hasImpacted = True
                                    }),
                                    bulletColor = bColor,
                                    turretColor = tColor,
                                    bulletRotation = bRotate,
                                    turretThickness = tTurr,
                                    lengthOfTurret = lTurr
                                    })
                                    else (WeaponGraphics {
                                    weaponPhysics = (GenericWeapon {
                                        currentPosition = trace("if else else x : " ++ show x ++ " y : " ++ show y ++ "\n") getPositionProjectile (Position x y) velocity theta,
                                        currentVelocity = getVelocityProjectile velocity theta,
                                        velocityMultiplyingFactor = f,
                                        currentAngle = getAngleProjectile velocity theta, 
                                        impactRadius = r,
                                        isLaunched = islaunched,
                                        hasImpacted = hasimpacted
                                    }),
                                    bulletColor = bColor,
                                    turretColor = tColor,
                                    bulletRotation = bRotate,
                                    turretThickness = tTurr,
                                    lengthOfTurret = lTurr
                                    })
                 else (WeaponGraphics {
                    weaponPhysics = (GenericWeapon {
                        currentPosition = trace("else x : " ++ show x ++ " y : " ++ show y ++ "\n") (Position x y),
                        currentVelocity = velocity,
                        velocityMultiplyingFactor = f,
                        currentAngle = theta, 
                        impactRadius = r,
                        isLaunched = islaunched,
                        hasImpacted = hasimpacted
                    }),
                    bulletColor = bColor,
                    turretColor = tColor,
                    bulletRotation = bRotate,
                    turretThickness = tTurr,
                    lengthOfTurret = lTurr
                    })

updateWeapon :: WeaponGraphics -> [[Tile]] -> WeaponGraphics 
updateWeapon weapon tileMap = if hasImpacted (weaponPhysics weapon)
                                then weapon
                                else updatePositionWeapon weapon tileMap 

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
             newWeapon = updateWeapon firedWeapon t
             newWeaponList = changeListElementAtIndex w weaponChoice newWeapon
         in GameState { tileMatrix = t , 
                        tankList =  l,
                        weapon = newWeaponList,
                        chance = if (isLaunched $ weaponPhysics newWeapon) then c else ((c+1) `mod` 2) ,
                        noOfPlayers = n,
                        isAcceptingInput = if (isLaunched $ weaponPhysics newWeapon) then False else True
                      }