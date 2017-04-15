module Tank where

import Types
import Physics
import Weapon
import Input 
import Common
import qualified Graphics.UI.GLUT
import Data.IORef


-- | Function to initialize the state variables of the tank
initializeTankState :: Float -> Float -> Float -> TankState
initializeTankState posX posY inclineangle = TankState {direction = FacingRight, 
                                     position = (originPosition posX posY),
                                     velocity = restVelocity,
                                     inclineAngle = inclineangle,
                                     turret = Turret {angle = 0.7853981633974483 , power = 1}
                                    }

-- | Function to initialize all the properties of a tank
initializeTank :: Float -> Float -> Float -> Float -> Graphics.UI.GLUT.Color4 Float -> Int -> [Integer] -> Tank
initializeTank posX posY score  initialangle tankcolor currweapon listweaponcount = Tank {tankState = (initializeTankState posX posY initialangle),
                                    score = score,
                                    color = tankcolor,
                                    currentWeapon = currweapon,
                                    weaponCount = listweaponcount
                                   } 

-- | Function responsible for launching a weapon. It takes a weapon, tank, the tile map
--   and the start velocity and returns the launched weapon
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
            launchVelocity =  (turret_power*startVelocity*f),
            launchAngle = (incline_theta+turret_theta),
            velocityMultiplyingFactor = f,
            currentAngle = (incline_theta+turret_theta), 
            impactRadius = radius,
            isLaunched = True,
            hasImpacted = False,
            launchDirection = (if (incline_theta+turret_theta) <= pi/2 then FacingRight else FacingLeft)
        }),
        bulletColor = bColor,
        turretColor = tColor,
        bulletRotation = bRotate,
        turretThickness = tTurr,
        lengthOfTurret = lTurr
    }

-- | Function to change the weapon count of the tank. It accepts a tank and returns a tank
--   with the modified weapon count
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

-- | To represent the velocity given to a tank on pressing a movement key
tankVelocity :: Float
tankVelocity = 1

-- | Function to accept the position, angle of inclination and key press and return
--   the new position of the tank    
updatePosition :: Point -> Float -> Key -> Point
updatePosition position theta key
    | key == moveRight = constantVelocityNewPosition position tankVelocity theta
    | key == moveLeft = constantVelocityNewPosition position (-tankVelocity) theta
    | otherwise = position

-- | Function to accept the power and key press and return the new power of the tank
updatePower :: Float -> Key -> Float
updatePower power key
    | (key == increasePower) = if(power+powerIncrement > 100) then 100 else power+powerIncrement
    | (key == decreasePower) = if(power-powerIncrement < 1) then 1 else power-powerIncrement
    | otherwise = power

-- | Function to accept the angle of inclination and key press and return the new angle
--   of inclination of the tank
updateAngle :: Float -> Key -> Float
updateAngle angle key 
    | key ==  decreaseAngle = if(angle+angleIncrement > pi) then pi else angle+angleIncrement
    | key == increaseAngle = if(angle-angleIncrement < 0) then 0 else angle-angleIncrement  -- check for negative
    | otherwise = angle

-- | Function to accept the direction and key press and return the new direction
--   of the tank
updateDirection :: Direction -> Key -> Direction
updateDirection direction key
    | key == moveRight = FacingRight 
    | key == moveLeft = FacingLeft
    | otherwise = direction

-- | Function to accept the current weapon selected and the key press and return
--   the new weapon to be selected for the tank
updateWeaponChoice :: Int -> Key -> Int
updateWeaponChoice currWeapon key
    | key == weapon0 = 0 
    | key == weapon1 = 1
    | key == weapon2 = 2
    | otherwise = currWeapon

-- | Function to update the parameters of a tank after a key press
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
    }) key tileMatrix = let newTankPosition = updatePosition (Position x y) (getAngleAt (Position x y) widthOfTank heightOfTank tileMatrix) key
                            isPositionValid = checkIfNotValidPosition newTankPosition widthOfTank heightOfTank incline_theta tileMatrix
                            newValidTankPosition = if (not isPositionValid) then newTankPosition else (Position (getPositionX newTankPosition) ((getPositionY newTankPosition)-1)) 
                            newTheta = getAngleAt newValidTankPosition widthOfTank heightOfTank tileMatrix
                            isThetaValid = checkThetaValidRange newTheta
                            newValidValidTankPosition = if(isThetaValid) then newValidTankPosition else (Position x y) 
                        in Tank {
        tankState = (TankState {
            direction = (updateDirection d key),
            position = newValidValidTankPosition ,
            velocity = (Velocity vx vy),
            inclineAngle = if(isThetaValid) then newTheta else incline_theta,
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

-- | Function to update the game state after current player launches the weapon
updateGameStateLaunchWeapon :: GameState -> GameState
updateGameStateLaunchWeapon
    (GameState {
        tileMatrix = t,
        tankList = l,
        weapon = w,
        chance = c,
        noOfPlayers = n
    }) = let weaponChoice = currentWeapon (l !! c)
             launched = launchWeapon (w !! weaponChoice) (l !! c) t 1
             newWeaponList = changeListElementAtIndex w weaponChoice launched
             newTankList = changeListElementAtIndex l c (decreaseWeaponCount (l !! c))
         in GameState {tileMatrix = t , tankList = newTankList, weapon = newWeaponList , chance = c , noOfPlayers=n, isAcceptingInput= False}

-- | Function to update the game state when player moves the tank
updateGameStateTank :: GameState -> Key -> GameState
updateGameStateTank
    (GameState {
        tileMatrix = t,
        tankList = l,
        weapon = w,
        chance = c,
        noOfPlayers = n,
        isAcceptingInput = d
    }) key = let temp = changeListElementAtIndex l c (updateTank (l !! c) key t)
        in GameState {tileMatrix = t , tankList =  temp, weapon = w , chance = c , noOfPlayers = n, isAcceptingInput = d}


-- | Function to update the game state after applying the effects of gravity on all the tanks
updateTankGravity :: GameState -> GameState
updateTankGravity
    (GameState {
        tileMatrix = t,
        tankList = l,
        weapon = w,
        chance = c,
        noOfPlayers = n,
        isAcceptingInput = d
    }) = GameState {tileMatrix = t , 
                    tankList =  (map (applyGravityOnAll t) l) , 
                    weapon = w , chance = c , 
                    noOfPlayers = n, 
                    isAcceptingInput = d
                  }
