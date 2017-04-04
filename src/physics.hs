data Point = Position Double Double | Velocity Double Double | Acceleration Double Double deriving (Show, Eq)

g :: Double
g = 9.8

gAcceleration :: Point
gAcceleration = (Acceleration 0 g)

unitTime :: Double
unitTime = 1

restVelocity :: Double
restVelocity = (Velocity 0 0)

originPosition :: Point
originPosition = (Position 0 0)

newPosition :: Point -> Point -> Point -> Double -> Point
newPosition (Position x y) (Velocity vx vy) (Acceleration ax ay) time = 
	(Position (newOneDPosition x vx ax time) (newOneDPosition y vy ay time)
	
newOneDPosition :: Double -> Double -> Double -> Double
newOneDPosition x v a time = x + v * time + 0.5 * a * time * time
	
cosComponent :: Double -> Double -> Double
cosComponent quantity theta = quantity * (cosine theta)

sinComponent :: Double -> Double -> Double
sinComponent quantity theta = quantity * (sine theta)

getComponentsVelocity :: Double -> Double -> Point
getComponentsVelocity quantity theta = (Velocity (cosComponent quantity theta) (sinComponent quantity theta))

gravityNewPosition :: Point -> Point -> Double -> Point
gravityNewPosition (Position x y) (Velocity vx vy) time =
	newPosition (Position x y) (Velocity vx vy) gAcceleration time
	
gravityNewPositionFromRest :: Point -> Double -> Point
gravityNewPositionFromRest (Position x y) time = gravityNewPosition (Position x y) restVelocity time

getNewPositionUnderGravity :: Point -> Double -> Double -> Double -> Point
getNewPositionUnderGravity (Position x y) velocity theta time =
	gravityNewPosition (Position x y) (getComponentsVelocity velocity theta) time
	
newPositionGravityFrame :: Point -> Double -> Double -> Point
newPositionGravityFrame (Position x y) velocity theta =
	getNewPositionUnderGravity (Position x y) velocity theta unitTime