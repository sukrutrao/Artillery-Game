module Tank where

import Types
import Physics
import Weapon
import Input 
import qualified Graphics.UI.GLUT
import Data.IORef

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

launchWeapon :: WeaponGraphics -> Tank -> [[Tile]] -> Float -> WeaponGraphics
launchWeapon
    (WeaponGraphics {
        weaponPhysics = (GenericWeapon {
            velocityMultiplyingFactor = f,
            impactRadius = radius
        }),
        bulletColor = bColor,
        turretColor = tColor,
        bulletRotation = bRotate,
        turretThickness = tTurr,
        lengthOfTurret = lTurr
    })
    
    (Tank {
        tankState = (TankState {
            position = (Position x y),
            inclineAngle = incline_theta,
            turret = (Turret {
                angle = turret_theta, 
                power = turret_power
            })
        })
    }) tileMap startVelocity = let topCenterX = x+((hypotenuseRect*cos(incline_theta+rectHalfAngle))/widthOfTile)
                                   topCenterY = y-((hypotenuseRect*sin(incline_theta+rectHalfAngle))/heightOfTile)
                                   turretTopX = topCenterX+((lTurr*cos(incline_theta+turret_theta))/heightOfTile)
                                   turretTopY = topCenterY-((lTurr*sin(incline_theta+turret_theta))/heightOfTile)
                                in WeaponGraphics {
        weaponPhysics = (GenericWeapon {
            currentPosition = (Position turretTopX turretTopY),
            currentVelocity = (turret_power*startVelocity*f),
            velocityMultiplyingFactor = f,
            currentAngle = (incline_theta+turret_theta), 
            impactRadius = radius,
            isLaunched = True,
            hasImpacted = False
        }),
        bulletColor = bColor,
        turretColor = tColor,
        bulletRotation = bRotate,
        turretThickness = tTurr,
        lengthOfTurret = lTurr
    }

decreaseWeaponCount :: Tank -> Tank
decreaseWeaponCount (Tank {
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
        weaponCount = changeListElementAtIndex f e ((f!!e)-1)
    }

tankVelocity :: Float
tankVelocity = 1
    
updatePosition :: Point -> Float -> Key -> Point
updatePosition position theta key
    | key == moveRight = constantVelocityNewPosition position tankVelocity theta
    | key == moveLeft = constantVelocityNewPosition position (-tankVelocity) theta
    | otherwise = position

updatePower :: Float -> Key -> Float
updatePower power key
    | (key == increasePower) = if(power+powerIncrement > 100) then 100 else power+powerIncrement
    | (key == decreasePower) = if(power-powerIncrement < 0) then 0 else power-powerIncrement
    | otherwise = power

updateAngle :: Float -> Key -> Float
updateAngle angle key
    | key == increaseAngle = if(angle+angleIncrement > pi) then pi else angle+angleIncrement
    | key == decreaseAngle = if(angle-angleIncrement < 0) then 0 else angle-angleIncrement  -- check for negative
    | otherwise = angle

updateDirection :: Direction -> Key -> Direction
updateDirection direction key
    | key == moveRight = FacingRight 
    | key == moveLeft = FacingLeft
    | otherwise = direction

updateWeaponChoice :: Int -> Key -> Int
updateWeaponChoice currWeapon key
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
            position = (updatePosition (Position x y) (getAngleAt (Position x y) widthOfTank tileMatrix) key),
            velocity = (Velocity vx vy),
            inclineAngle = (getAngleAt (Position x y) widthOfTank tileMatrix),--getAngleincline_theta,
            turret = (Turret {
                angle = (updateAngle turret_theta key), 
                power = (updatePower turret_power key)
            })
        }),
        score = s,
        color = c,
        currentWeapon = (updateWeaponChoice e key),
        weaponCount = f
    }

-- check for theta = pi/2 

updateGameStateLaunchWeapon :: GameState -> GameState
updateGameStateLaunchWeapon
    (GameState {
        tileMatrix = t,
        tankList = l,
        weapon = w,
        chance = c
    }) = let weaponChoice = currentWeapon (l !! c)
             launched = launchWeapon (w !! weaponChoice) (l !! c) t 1
             newWeaponList = changeListElementAtIndex w weaponChoice launched
             newTankList = changeListElementAtIndex l c (decreaseWeaponCount (l !! c))
         in GameState {tileMatrix = t , tankList = newTankList, weapon = newWeaponList , chance = c , isAcceptingInput = False}


updateGameStateTank :: GameState -> Key -> GameState
updateGameStateTank
    (GameState {
        tileMatrix = t,
        tankList = l,
        weapon = w,
        chance = c,
        isAcceptingInput = d
    }) key = let temp = changeListElementAtIndex l c (updateTank (l !! c) key t)
        in GameState {tileMatrix = t , tankList =  temp, weapon = w , chance = c , isAcceptingInput = d}


