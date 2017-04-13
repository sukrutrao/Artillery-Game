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

updatePositionWeapon :: Weapon -> [[Tile]] -> Weapon
updatePositionWeapon (GenericWeapon {
            currentPosition = (Position x y),
            currentVelocity = velocity,
            velocityMultiplyingFactor = f,
            currentAngle = theta, 
            impactRadius = r,
            isLaunched = islaunched,
            hasImpacted = hasimpacted,
            launchDirection = l
        }) tileMap = if islaunched then 
                                if (truncate y>((length tileMap)-2) || y<0) then (GenericWeapon {
                                        currentPosition = trace("if if x : " ++ show x ++ " y : " ++ show y ++ "\n") getPositionProjectile (Position x y) velocity theta,
                                        currentVelocity = getVelocityProjectile velocity theta,
                                        velocityMultiplyingFactor = f,
                                        currentAngle = getAngleProjectile velocity theta l,
                                        impactRadius = r,
                                        isLaunched = islaunched,
                                        hasImpacted = hasimpacted,
                                        launchDirection = l
                                    })
                                else if ((truncate x) == 0 || (truncate x) == (tileMatrixColumnSize-1)) then (GenericWeapon {
                                        currentPosition = (Position x y),
                                        currentVelocity = velocity,
                                        velocityMultiplyingFactor = f,
                                        currentAngle = theta, 
                                        impactRadius = r,
                                        isLaunched = False,
                                        hasImpacted = False,
                                        launchDirection = l
                                    })
                                    else if getIsObstacle tileMap y x then (GenericWeapon {
                                        currentPosition = trace("if else if x : " ++ show x ++ " y : " ++ show y ++ "\n") (Position x y),
                                        currentVelocity = velocity,
                                        velocityMultiplyingFactor = f,
                                        currentAngle = theta, 
                                        impactRadius = r,
                                        isLaunched = False,
                                        hasImpacted = True,
                                        launchDirection = l
                                    })
                                    else (GenericWeapon {
                                        currentPosition = trace("if else else x : " ++ show x ++ " y : " ++ show y ++ "\n") getPositionProjectile (Position x y) velocity theta,
                                        currentVelocity = getVelocityProjectile velocity theta,
                                        velocityMultiplyingFactor = f,
                                        currentAngle = getAngleProjectile velocity theta l,
                                        impactRadius = r,
                                        isLaunched = islaunched,
                                        hasImpacted = hasimpacted,
                                        launchDirection = l
                                    })
                 else (GenericWeapon {
                        currentPosition = trace("else x : " ++ show x ++ " y : " ++ show y ++ "\n") (Position x y),
                        currentVelocity = velocity,
                        velocityMultiplyingFactor = f,
                        currentAngle = theta, 
                        impactRadius = r,
                        isLaunched = islaunched,
                        hasImpacted = hasimpacted,
                        launchDirection = l
                    })

updateWeapon :: WeaponGraphics -> [[Tile]] -> WeaponGraphics 
updateWeapon (WeaponGraphics {
        weaponPhysics = (GenericWeapon {
            currentPosition = (Position x y),
            currentVelocity = velocity,
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
    })  tileMap = if hasimpacted then (WeaponGraphics {
        weaponPhysics = (GenericWeapon {
            currentPosition = (Position x y),
            currentVelocity = velocity,
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
                else (WeaponGraphics {
        weaponPhysics = updatePositionWeapon (GenericWeapon {
            currentPosition = (Position x y),
            currentVelocity = velocity,
            velocityMultiplyingFactor = f,
            currentAngle = theta, 
            impactRadius = r,
            isLaunched = islaunched,
            hasImpacted = hasimpacted,
            launchDirection = l
        }) tileMap,
        bulletColor = bColor,
        turretColor = tColor,
        bulletRotation = bRotate,
        turretThickness = tTurr,
        lengthOfTurret = lTurr
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
             newWeapon =  updateWeapon firedWeapon t
             shouldWeBlast =  if(isLaunched $ weaponPhysics newWeapon) then t
                                else if(hasImpacted $ weaponPhysics newWeapon)
                                        then makeTileNotObsAtPts t (getAllPointsInCircle (currentPosition (weaponPhysics newWeapon)) (impactRadius (weaponPhysics newWeapon)))
                                        else t
             newWeaponList = changeListElementAtIndex w weaponChoice newWeapon
         in GameState { tileMatrix = shouldWeBlast, 
                        tankList =  l,
                        weapon = newWeaponList,
                        chance = if (isLaunched $ weaponPhysics newWeapon) then c else ((c+1) `mod` n) ,
                        noOfPlayers = n,
                        isAcceptingInput = if (isLaunched $ weaponPhysics newWeapon) then False else True
                      }