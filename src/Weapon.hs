module Weapon where

import qualified Graphics.UI.GLUT
import Types
import Physics

defaultStartVelocity :: Float
defaultStartVelocity = 10

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



{-
updatePositionWeapon :: Weapon -> Weapon
updatePositionWeapon (GenericWeapon {
    	currentPosition = (Position x y),
    	currentVelocity = velocity,
    	currentAngle = theta, 
    	impactRadius = r,
    	isLaunched = islaunched,
    	hasImpacted = hasimpacted
	}) = if islaunched
			then (if (isObstacle (Position x y))
					then	(GenericWeapon {
    							currentPosition = (Position x y),
    							currentVelocity = velocity,
    							currentAngle = theta, 
    							impactRadius = r,
    							isLaunched = islaunched,
								hasImpacted = True
							})
					else	(GenericWeapon {
    							currentPosition = (getPositionProjectile (Position x y) velocity theta),
    							currentVelocity = (getVelocityProjectile velocity theta),
    							currentAngle = (getAngleProjectile velocity theta), 
    							impactRadius = r,
    							isLaunched = islaunched,
    							hasImpacted = hasimpacted
							})
					)
			else	(GenericWeapon {
    					currentPosition = (Position x y),
    					currentVelocity = velocity,
    					currentAngle = theta, 
    					impactRadius = r,
    					isLaunched = islaunched,
    					hasImpacted = hasimpacted
					})
					
updateWeapon :: Weapon -> Weapon 
updateWeapon weapon = if (not $ hasImpacted weapon)
										then (updatePositionWeapon weapon) 
										else weapon

-}
