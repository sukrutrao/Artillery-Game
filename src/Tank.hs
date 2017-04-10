module Tank where

import Types
import Physics
import Weapon
import Input 
import qualified Graphics.UI.GLUT
import Data.IORef


edgeOfTriangle :: Float
edgeOfTriangle = 0.075

widthOfTank :: Float
widthOfTank = 0.2

heightOfTank :: Float
heightOfTank = 0.1

powerIncrement :: Float
powerIncrement = 1

angleIncrement :: Float
angleIncrement = 1

initializeTankState :: Float -> Float -> TankState
initializeTankState x y = TankState {direction = FacingRight, 
                                     position = (originPosition x y),
                                     velocity = restVelocity,
                                     inclineAngle = 0,
                                     turret = Turret {angle = 45 , power = 0}
                                    }

initializeTank :: Float -> Float -> Integer -> Graphics.UI.GLUT.Color4 Float -> Int -> [Integer] -> Tank
initializeTank a b c d e f = Tank {tankState = (initializeTankState a b),
                                    score = c,
                                    color = d,
                                    currentWeapon = e,
                                    weaponCount = f
                                   } 
{-
launchWeapon :: Weapon -> Tank -> Float -> Float -> Weapon
launchWeapon
    (GenericWeapon {
        currentPosition = (Position px py),
        currentVelocity = v,
        currentAngle = theta, 
        impactRadius = r,
        isLaunched = l,
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
        score = _,
        color = _,

    }) startVelocity radius = (GenericWeapon (Position x y) startVelocity (incline_theta + turret_theta) radius True False)
-}
tankVelocity :: Float
tankVelocity = 10
    
updatePosition :: Point -> Float -> Key -> Point
updatePosition position theta key
    | key == moveRight = constantVelocityNewPosition position tankVelocity theta
    | key == moveLeft = constantVelocityNewPosition position (-tankVelocity) theta
    | otherwise = position

updatePower :: Float -> Key -> Float
updatePower power key
    | (key == increasePower) = if((power + powerIncrement) > 100) then 100 else (power + powerIncrement)
    | (key == decreasePower) = if((power - powerIncrement) < 0) then 0 else (power - powerIncrement)
    | otherwise = power

updateAngle :: Float -> Key -> Float
updateAngle angle key
    | key == increaseAngle = angle + angleIncrement
    | key == decreaseAngle = angle - angleIncrement -- check for negative
    | otherwise = angle

updateDirection :: Direction -> Key -> Direction
updateDirection direction key
    | key == moveRight = FacingRight 
    | key == moveLeft = FacingLeft
    | otherwise = direction


updateWeapon :: Int -> Key -> Int
updateWeapon currWeapon key
    | key == weapon0 = 0 
    | key == weapon1 = 1
    | key == weapon2 = 2
    | otherwise = currWeapon


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
        score = s,
        color = c,
        currentWeapon = e,
        weaponCount = f
        
    }) = Tank {
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
        score = s,
        color = c,
        currentWeapon = e,
        weaponCount = f
    }


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
        score = s,
        color = c,
        currentWeapon = e,
        weaponCount = f
    }) key = Tank {
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
        score = s,
        color = c,
        currentWeapon = (updateWeapon e key),
        weaponCount = f
    }

-- check for theta = pi/2!





updateGameStateTank :: GameState -> Key -> GameState
updateGameStateTank
    (GameState {
        tileMatrix = t,
        tankList = l,
        weapon = w,
        chance = c
    }) key = let temp = ((take c l) ++ ((updateTank (l !! c) key) : (drop (c + 1) l)))
        in GameState {tileMatrix = t , tankList =  temp, weapon = w , chance = c }


