module Weapon where

import Physics

data Weapon = GenericWeapon {
    currentPosition :: Point,
    currentVelocity :: Float,
    currentAngle :: Float, 
    impactRadius :: Float,
    isLaunched :: Bool
}

defaultStartVelocity :: Float
defaultStartVelocity = 10



	

	


