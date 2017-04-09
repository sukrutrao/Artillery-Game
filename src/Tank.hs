module Tank where

import Physics
import Weapon
import Input 
import qualified Graphics.UI.GLUT
import Data.IORef

data Turret = Turret {
    angle :: Float, 
    power :: Float  
} deriving (Show)

data Direction = FacingLeft | FacingRight  
           deriving (Enum , Show)

data TankState = TankState {
    direction :: Direction,
    position :: Point,
    velocity :: Point,
    inclineAngle :: Float,
    turret :: Turret
} deriving (Show)

data Tank = Tank {
    tankState :: TankState,
    tankWeapons :: [Weapon],
    score :: Integer,
    -- Attributes For Graphics
    color :: Graphics.UI.GLUT.Color4 Float,
    healthBarPosition :: Point
} deriving (Show)

edgeOfTriangle :: Float
edgeOfTriangle = 0.075

widthOfTank :: Float
widthOfTank = 0.2

heightOfTank :: Float
heightOfTank = 0.15

lengthOfTurret :: Float
lengthOfTurret = 0.1

powerIncrement :: Float
powerIncrement = 1

angleIncrement :: Float
angleIncrement = 1

initializeTankState :: Float -> Float -> TankState
initializeTankState x y = TankState {direction = FacingRight, position = (originPosition x y) , velocity = restVelocity , inclineAngle = 0 , turret = Turret {angle = 45 , power = 40}}

initializeTank :: Float -> Float -> Graphics.UI.GLUT.Color4 Float -> Point -> Tank
initializeTank x y z o = Tank {tankState = (initializeTankState x y), tankWeapons = [initializeWeapon x y], score = 25 , color = z , healthBarPosition = o} 

launchWeapon :: Weapon -> Tank -> Float -> Float -> Weapon
launchWeapon
    (GenericWeapon {
        currentPosition = (Position px py),
        currentVelocity = v,
        currentAngle = theta, 
        impactRadius = r,
        isLaunched = l
    	hasImpacted = i
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
        score = s,
        color = _,
        healthBarPosition = _
    }) startVelocity radius = (GenericWeapon (Position x y) startVelocity (incline_theta + turret_theta) radius True False)

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
        score = s,
        color = c,
        healthBarPosition = p
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
        score = s,
        color = c,
        healthBarPosition = p
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
        score = s,
        color = c,
        healthBarPosition = p
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
        score = s,
        color = c,
        healthBarPosition = p
    })
    
-- check for theta = pi/2!
