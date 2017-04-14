module Common where

import Types
import Physics
import Debug.Trace
applyGravityOnAll :: [[Tile]] -> Tank -> Tank
applyGravityOnAll tileMatrix (Tank {
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
    })  = let gravityPosition = (tankGravityNewPosition (Position x y) widthOfTank heightOfTank incline_theta tileMatrix)
                        in Tank {
        tankState = (TankState {
            direction = d,
            position =  trace("GRAVITY HERE , X : " ++ show x ++ " , Y : " ++ show y ++ " , grav : " ++ show gravityPosition)  (gravityPosition),
            velocity = (Velocity vx vy),
            inclineAngle = getAngleAt gravityPosition widthOfTank heightOfTank tileMatrix,--getAngleincline_theta,
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