module Weapon where

import Types
import Physics

defaultStartVelocity :: Float
defaultStartVelocity = 10

genericImpactRadius :: Float
genericImpactRadius = 10

initializeWeapon :: Float -> Float -> Weapon
initializeWeapon x y = GenericWeapon {currentPosition = (originPosition x y) , currentVelocity = defaultStartVelocity, currentAngle = 0 , impactRadius = genericImpactRadius , isLaunched = False, hasImpacted = False}

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
updateWeapon (GenericWeapon weapon) = if (not $ hasImpacted weapon)
										then (updatePositionWeapon weapon) 
										else weapon

-}