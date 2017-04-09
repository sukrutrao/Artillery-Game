module Physics where

data Point = Position Float Float | Velocity Float Float | Acceleration Float Float deriving (Show, Eq)

g :: Float
g = 9.8

gAcceleration :: Point
gAcceleration = (Acceleration 0 (-g))

unitTime :: Float
unitTime = 1

restVelocity :: Point
restVelocity = (Velocity 0 0)

originPosition :: Float -> Float -> Point
originPosition x y = (Position x y)

newPosition :: Point -> Point -> Point -> Float -> Point
newPosition (Position x y) (Velocity vx vy) (Acceleration ax ay) time =
    (Position (newOneDPosition x vx ax time) (newOneDPosition y vy ay time))

newVelocity :: Point -> Point -> Float -> Point
newVelocity (Velocity vx vy) (Acceleration ax ay) time = 
    (Velocity (newOneDVelocity vx ax time) (newOneDVelocity vy ay time))

newOneDPosition :: Float -> Float -> Float -> Float -> Float
newOneDPosition x v a time = x + v * time + 0.5 * a * time * time

newOneDVelocity :: Float -> Float -> Float -> Float
newOneDVelocity v a time = v + a * time

cosComponent :: Float -> Float -> Float
cosComponent quantity theta = quantity * cos(theta)

sinComponent :: Float -> Float -> Float
sinComponent quantity theta = quantity * sin(theta)

getComponentsVelocity :: Float -> Float -> Point
getComponentsVelocity quantity theta = (Velocity (cosComponent quantity theta) (sinComponent quantity theta))

gravityNewPosition :: Point -> Point -> Float -> Point
gravityNewPosition (Position x y) (Velocity vx vy) time =
    newPosition (Position x y) (Velocity vx vy) gAcceleration time

newPositionVTheta :: Point -> Point -> Float -> Float -> Point
newPositionVTheta position velocity acceleration theta = newPosition position velocity (Acceleration acceleration g) unitTime

gravityNewPositionFromRest :: Point -> Float -> Point
gravityNewPositionFromRest (Position x y) time = gravityNewPosition (Position x y) restVelocity time

getNewPositionUnderGravity :: Point -> Float -> Float -> Float -> Point
getNewPositionUnderGravity (Position x y) velocity theta time =
    gravityNewPosition (Position x y) (getComponentsVelocity velocity theta) time

newPositionGravityFrame :: Point -> Float -> Float -> Point
newPositionGravityFrame (Position x y) velocity theta =
    getNewPositionUnderGravity (Position x y) velocity theta unitTime

constantVelocityNewPosition :: Point -> Float -> Float ->  Point
constantVelocityNewPosition position velocity theta = newPosition position (getComponentsVelocity velocity theta) (Acceleration 0 0) unitTime

getAngleProjectile :: Float -> Float -> Float 
getAngleProjectile velocity theta = atan(tan(theta) - (g * t * sec(theta))/u)

getPositionProjectile :: Point -> FLoat -> Float -> Point 
getPositionProjectile position velocity theta = getNewPositionUnderGravity position velocity theta unitTime

getVelocityProjectile :: Float -> Float -> Float
getVelocityProjectile velocity theta = sqrt((velocity * cos(theta))^2 + (velocity * sin(theta) - gt)^2)
