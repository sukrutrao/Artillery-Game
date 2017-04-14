
module Weapon where

import qualified Graphics.UI.GLUT
import Types
import Physics
import Common
import Debug.Trace

-- |The defaultStartVelocity function sets the default start velocity for the bullet.
defaultStartVelocity :: Float
defaultStartVelocity = 1

-- |The genericImpactRadius function sets the default impact radius for the bullet.
genericImpactRadius :: Float
genericImpactRadius = 10

-- |The initializeWeapon function initializes the data type WeaponGraphics with the given values
initializeWeapon :: Float   -- ^ Initial Column Number in the tileMatrix
                 -> Float   -- ^ Initial Row Number in the tileMatrix
                 -> Float   -- ^ The Initial Velocity Multiplying Factor
                 -> Float   -- ^ The Initial inclination angle of weapon
                 -> Float   -- ^ Weapon's Impact Radius
                 -> Graphics.UI.GLUT.Color4 Float   -- ^ Bullet Color
                 -> Graphics.UI.GLUT.Color4 Float   -- ^ Bullet Color
                 ->  Graphics.UI.GLUT.Vector3 Float   -- ^ Initial Column Number in the tileMatrix
                 -> Float   -- ^ Initial Column Number in the tileMatrix
                 -> Float   -- ^ Initial Column Number in the tileMatrix
                 -> WeaponGraphics   -- ^ Initial Column Number in the tileMatrix
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
                                        hasImpacted = False,
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
        color = c,
        currentWeapon = e,
        weaponCount = f
    }) =  let scoreDiff =  trace ("SCORE DIFF : " ++ show (getAllPointsInRectangleHelper (Position x y) widthOfTank heightOfTank incline_theta 0) ++ " " ++ show incline_theta++ " " ++ show widthOfTank ++ " " ++ show heightOfTank ++ " " ++ show (Position x y)) (fromIntegral (length $ commonPointsBetweenLists (convertPointListToInteger $  getAllPointsInCircle (Position wx wy) impactradius) (convertPointListToInteger $ getAllPointsInRectangle (Position x y) widthOfTank heightOfTank incline_theta)) * weapon_velocity * 0.01)
          in (Tank {
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
        score = trace("Score set") (s - scoreDiff),
        color = trace("colour") (if(s-scoreDiff<=0) then Graphics.UI.GLUT.Color4 1 0 0 0  else c),
        currentWeapon = trace("CW") (e),
        weaponCount = trace("WC") (f)
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
             updatedTankHealth = if(isLaunched $ weaponPhysics newWeapon) then l
                                    else if(not $ hasImpacted $ weaponPhysics newWeapon) then l
                                            else trace ("Inside GRAVITY : ") (map (applyGravityOnAll shouldWeBlast) (map (updateHealth (weaponPhysics newWeapon)) l))
         in GameState { tileMatrix = trace ("Inside Should we blast : ") (shouldWeBlast), 
                        tankList =  updatedTankHealth,
                        weapon = trace("nwl") (newWeaponList),
                        chance = trace("C") (if (isLaunched $ weaponPhysics newWeapon) then c else if(noOfPlayerWithNoHealth updatedTankHealth >= n-1) then 0 else (nextTankChance updatedTankHealth c n)),
                        noOfPlayers = n,
                        isAcceptingInput = if(noOfPlayerWithNoHealth updatedTankHealth >= n-1)
                                                then  trace ("Inside isAcceptingInput condtion 1 ")  (False)
                                                else if (isLaunched $ weaponPhysics newWeapon) 
                                                        then  trace ("Inside isAcceptingInput condtion 2 ") (False)
                                                        else  trace ("Inside isAcceptingInput condtion 3 ") (True)
                      }


noOfPlayerWithNoHealth::[Tank] -> Int
noOfPlayerWithNoHealth tankList = length $ filter checkScore tankList

checkScore :: Tank -> Bool
checkScore tank = if (score tank <= 0) then True else False

nextTankChance::[Tank] -> Int -> Int -> Int
nextTankChance tankList currChance noOfPlayers = let nextChance =(currChance+1) `mod` noOfPlayers
                                             in if(score (tankList !! nextChance) <= 0)
                                                    then nextTankChance tankList nextChance noOfPlayers
                                                    else nextChance
