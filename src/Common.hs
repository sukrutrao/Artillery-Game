module Common where

import Types
import Physics

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
            position = gravityPosition,
            velocity = (Velocity vx vy),
            inclineAngle = getAngleAt gravityPosition widthOfTank tileMatrix,--getAngleincline_theta,
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