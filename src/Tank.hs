module Tank where

import Types
import Physics
import Weapon
import Input 
import qualified Graphics.UI.GLUT
import Data.IORef


edgeOfTriangle :: Float
edgeOfTriangle = 0.075

widthOfTank :: Integer
widthOfTank = 4

heightOfTank :: Integer
heightOfTank = 2

powerIncrement :: Float
powerIncrement = 1

angleIncrement :: Float
angleIncrement = 0.1

initializeTankState :: Float -> Float -> TankState
initializeTankState posX posY = TankState {direction = FacingRight, 
                                     position = (originPosition posX posY),
                                     velocity = restVelocity,
                                     inclineAngle = 0,
                                     turret = Turret {angle = 0.7853981633974483 , power = 0}
                                    }

initializeTank :: Float -> Float -> Integer -> Graphics.UI.GLUT.Color4 Float -> Int -> [Integer] -> Tank
initializeTank posX posY score tankcolor currweapon listweaponcount = Tank {tankState = (initializeTankState posX posY),
                                    score = score,
                                    color = tankcolor,
                                    currentWeapon = currweapon,
                                    weaponCount = listweaponcount
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


updateTank :: Tank -> Key -> [[Tile]] -> Tank
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
    }) key tileMatrix = Tank {
        tankState = (TankState {
            direction = (updateDirection d key),
            position = (Position x y),
           -- position = (updatePosition (Position x y) (getAngleAt (Position x y) widthOfTank tileMatrix) key),
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
    }) key = let temp = ((take c l) ++ ((updateTank (l !! c) key t) : (drop (c + 1) l)))
        in GameState {tileMatrix = t , tankList =  temp, weapon = w , chance = c }


