module Tank where

import Physics
import Weapon
import qualified Graphics.UI.GLUT
import Data.IORef

data Turret = Turret {
	angle :: Float, 
    power :: Float  
}

data Direction = FacingLeft | FacingRight  
           deriving (Enum)

data TankState = TankState {
	direction :: Direction,
	position :: Point,
	velocity :: Point,
	inclineAngle :: Float,
	turret :: Turret
}

data Tank = Tank {
	tankState :: TankState,
	tankWeapons :: [Weapon],
	score :: Integer
}

powerIncrement :: Float
powerIncrement = 1

angleIncrement :: Float
angleIncrement = 1

launchWeapon :: Weapon -> Tank -> Float -> Float -> Weapon
launchWeapon
	(GenericWeapon {
		currentPosition = (Position px py),
		currentVelocity = v,
		currentAngle = theta, 
		impactRadius = r,
		isLaunched = l
	})
	(Tank {
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
		tankWeapons = w,
		score = s
	}) startVelocity radius = (GenericWeapon (Position x y) startVelocity (incline_theta + turret_theta) radius True)

                  
                     
