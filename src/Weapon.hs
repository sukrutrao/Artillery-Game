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

initializeWeapon :: Float -> Float -> Weapon
initializeWeapon x y = GenericWeapon {currentPosition = (originPosition x y) , currentVelocity = defaultStartVelocity, currentAngle = 0 , impactRadius = 0 , isLaunched = False}


