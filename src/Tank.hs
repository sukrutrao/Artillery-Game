module Tank where

import Physics
import Weapon
import Input 
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

initializeTankState :: Float -> Float -> TankState
initializeTankState x y = TankState {direction = FacingRight, position = (originPosition x y) , velocity = restVelocity , inclineAngle = 0 , turret = Turret {angle = 0 , power = 0}}

initializeTank :: Float -> Float -> Tank
initializeTank x y = Tank {tankState = (initializeTankState x y), tankWeapons = [initializeWeapon x y], score = 30}

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
    
tankVelocity :: Float
tankVelocity = 10
    
updatePosition :: Point -> Float -> Key -> Point
updatePosition position theta key
    | key == moveRight = constantVelocityNewPosition position tankVelocity theta
    | key == moveLeft = constantVelocityNewPosition position (-tankVelocity) theta
    | otherwise = position

{-
updatePosition position theta moveRight = 
updatePosition position theta moveLeft = constantVelocityNewPosition position (-tankVelocity) theta
updatePosition position theta key = position
-}

updatePower :: Float -> Key -> Float
updatePower power key
    | key == increasePower = power + powerIncrement
    | key == increasePower = power - powerIncrement -- check for negative
    | otherwise = power

{-
updatePower :: Float -> Key -> Float
updatePower power increasePower = power + powerIncrement
updatePower power increasePower = power - powerIncrement -- check for negative
updatePower power key = power
-}

updateAngle :: Float -> Key -> Float
updateAngle angle key
    | key == increaseAngle = angle + angleIncrement
    | key == decreaseAngle = angle - angleIncrement -- check for negative
    | otherwise = angle

{-
updateAngle :: Float -> Key -> Float
updateAngle angle increaseAngle = angle + angleIncrement
updateAngle angle decreaseAngle = angle - angleIncrement -- check for negative
updateAngle angle key = angle
-}


updateDirection :: Direction -> Key -> Direction
updateDirection direction key
    | key == moveRight = FacingRight 
    | key == moveLeft = FacingLeft
    | otherwise = direction

{-
updateDirection :: Direction -> Key -> Direction
updateDirection direction moveRight = FacingRight 
updateDirection direction moveLeft = FacingLeft
updateDirection direction key = direction
-}


stopTank :: Tank -> Tank
stopTank (Tank {
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
    }) = (Tank {
        tankState = (TankState {
            direction = d,
            position = (Position x y),
            velocity = (Velocity 0 0),
            inclineAngle = incline_theta,
            turret = (Turret {
                angle = turret_theta, 
                power = turret_power
            })
        }),
        tankWeapons = w,
        score = s
    })
    


updateTank :: Tank -> Key -> Tank
updateTank
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
    }) key = (Tank {
        tankState = (TankState {
            direction = (updateDirection d key),
            position = (updatePosition (Position x y) (getAngleAt (Position x y)) key),
            velocity = (Velocity vx vy),
            inclineAngle = incline_theta,
            turret = (Turret {
                angle = (updateAngle turret_theta key), 
                power = (updatePower turret_power key)
            })
        }),
        tankWeapons = w,
        score = s
    })
