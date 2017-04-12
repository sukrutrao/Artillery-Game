module Physics where

import Types
import Debug.Trace

g :: Float
g = 1

gAcceleration :: Point
gAcceleration = (Acceleration 0 g)

unitTime :: Float
unitTime = 0.5

restVelocity :: Point
restVelocity = (Velocity 0 0)

originPosition :: Float -> Float -> Point
originPosition x y = (Position x y)

newPosition :: Point -> Point -> Point -> Float -> Point
newPosition (Position x y) (Velocity vx vy) (Acceleration ax ay) time =
    (Position (newOneDPosition x vx ax time) (newOneDPosition y (-vy) ay time))

newVelocity :: Point -> Point -> Float -> Point
newVelocity (Velocity vx vy) (Acceleration ax ay) time = 
   -- -- trace("vx in physics : " ++ show vx ++ "vy in physics : " ++ show vy ++ "\n")
    (Velocity (newOneDVelocity vx ax time) (newOneDVelocity (-vy) ay time))

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
    -- trace("vx component : " ++ show vx ++ " vy component : " ++ show vy ++ "\n")
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

getIsObstacle:: [[Tile]] -> Float -> Float -> Bool
getIsObstacle tileMatrix row col = (isObstacle ((tileMatrix !! (truncate row)) !! (truncate col)))


getTilePos:: [[Tile]] -> Float -> Float -> Point
getTilePos tileMatrix row col = tilePosition ((tileMatrix !! (truncate row)) !! (truncate col))

getTilePosX:: [[Tile]] -> Float -> Float -> Float
getTilePosX tileMatrix row col = getPositionX (tilePosition ((tileMatrix !! (truncate row)) !! (truncate col)))

getTilePosY:: [[Tile]] -> Float -> Float -> Float
getTilePosY tileMatrix row col = getPositionY (tilePosition ((tileMatrix !! (truncate row)) !! (truncate col)))

getPositionX:: Point -> Float
getPositionX (Position x _) = x

getPositionY:: Point -> Float
getPositionY (Position _ y) = y

getAngleProjectile :: Float -> Float -> Direction -> Float 
getAngleProjectile velocity theta FacingRight = trace ("R+++++++++++++ theta: " ++ show theta ++ "  ,  velocity : " ++ show velocity ++ "\n") ((-1) * atan((-1) * tan(theta) + (g * unitTime)/(velocity * cos(theta))))
getAngleProjectile velocity theta FacingLeft = trace ("L+++++++++++++ theta: " ++ show theta ++ "  ,  velocity : " ++ show velocity ++ "\n") (pi  + (-1) * atan((-1) * tan(theta) + (g * unitTime)/(velocity * cos(theta))))

getPositionProjectile :: Point -> Float -> Float -> Point 
getPositionProjectile position velocity theta = getNewPositionUnderGravity position velocity theta unitTime

getVelocityProjectile :: Float -> Float -> Float
getVelocityProjectile velocity theta = sqrt((velocity * cos(theta))^2 + ((-1) * velocity * sin(theta) + g * unitTime)^2)

data PointLineOrientation = AboveLine | BelowLine deriving(Enum, Eq) -- what about 90 degrees and its multiples?

getLineSlopeIntercept :: Point -> Point -> (Float,Float)
getLineSlopeIntercept (Position x1 y1) (Position x2 y2) = (((y2-y1)/(x2-x1)), y1-(x1*((y2-y1)/(x2-x1))))

checkOrientationPointLine :: Point -> Point -> Point -> PointLineOrientation
checkOrientationPointLine (Position x y) first second
    |    y > x*fst(getLineSlopeIntercept first second) + snd(getLineSlopeIntercept first second) = AboveLine
    |    otherwise = BelowLine -- for multiples of 90 degrees?
    
checkPointInRectangle :: Point -> Point -> Float -> Float -> Float -> Bool
checkPointInRectangle point (Position lx ly) length width theta = 
    if ((checkOrientationPointLine point (Position lx ly) (Position (lx + length * cos(theta)) (ly - length * sin(theta)))) == AboveLine &&
        (checkOrientationPointLine point (Position lx ly) (Position (lx - width * sin(theta)) (ly - width * cos(theta)))) == AboveLine &&
        (checkOrientationPointLine point (Position (lx - width * sin(theta) + length * cos(theta)) (ly - width * cos(theta) - length * sin(theta))) (Position (lx + length * cos(theta)) (ly - length * sin(theta)))) == BelowLine &&
        (checkOrientationPointLine point (Position (lx - width * sin(theta) + length * cos(theta)) (ly - width * cos(theta) - length * sin(theta))) (Position (lx - width * sin(theta)) (ly - width * cos(theta)))) == BelowLine)
        then True
        else False
        
checkPointInCircle :: Point -> Point -> Float -> Bool
checkPointInCircle (Position x y) (Position cx cy) radius
    |    (x-cx)^2 + (y-cy)^2 <= radius^2 = True -- trace("Tx,y,cx,cy,radius : " ++ show x ++ show y ++ show cx ++ show cy ++ show radius ++ "\n") True
    |    otherwise = False -- trace("Fx,y,cx,cy,radius : " ++ show x ++ show y ++ show cx ++ show cy ++ show radius ++ "\n") 
    
getListOfPointsInCircle :: Point -> Float -> [[Point]] -> [Point]
getListOfPointsInCircle (Position cx cy) radius [] = []
getListOfPointsInCircle (Position cx cy) radius [[]] = []
getListOfPointsInCircle (Position cx cy) radius (x:xs) =  -- trace(show xs)
    ((checkCommonPointsCircleLine (Position cx cy) radius x) ++ (getListOfPointsInCircle (Position cx cy) radius xs))

getAllPointsInCircle :: Point -> Float -> [Point]
getAllPointsInCircle (Position cx cy) radius = 
	getListOfPointsInCircle (Position cx cy) radius $ getListOfPointsInRectangle (Position (cx - radius) (cy - radius)) (truncate (2*radius)) (truncate (2*radius)) 

checkCommonPointsCircleLine :: Point -> Float -> [Point] -> [Point]
checkCommonPointsCircleLine (Position cx cy) radius [] = []
checkCommonPointsCircleLine (Position cx cy) radius (x:xs) = 
    if (checkPointInCircle x (Position cx cy) radius) then (x : (checkCommonPointsCircleLine (Position cx cy) radius xs))
    else (checkCommonPointsCircleLine (Position cx cy) radius xs)
        
    
getListOfPointsInLine :: Point -> Integer -> [Point]
getListOfPointsInLine (Position x y) length
    |    length < 0 = []
    |    otherwise = ((Position x y) : (getListOfPointsInLine (Position (x+1) y) (length - 1)))
    
getListOfPointsInRectangle :: Point -> Integer -> Integer -> [[Point]]
getListOfPointsInRectangle (Position x y) length width
    |    width < 0 = [[]]
    |    otherwise = ((getListOfPointsInLine (Position x y) length) : (getListOfPointsInRectangle (Position x (y+1)) length (width-1)))
    
flattenList :: [[Point]] -> [Point]
flattenList [] = []
flattenList [[]] = []
flattenList (x:xs) = (x ++ (flattenList xs))
    
commonPointsBetweenLists :: [Point] -> [Point] -> [Point]
commonPointsBetweenLists [] [] = [] 
commonPointsBetweenLists (x:xs) [] = []
commonPointsBetweenLists [] y = []
commonPointsBetweenLists (x:xs) y = if (x `elem` y) then (x : (commonPointsBetweenLists xs y))
                                    else (commonPointsBetweenLists xs y)

                              
commonPointsBetweenCircleRectangle :: Point -> Float -> Point -> Float -> Float -> [Point]
commonPointsBetweenCircleRectangle (Position cx cy) radius (Position x y) length width = 
    (commonPointsBetweenLists (flattenList $ (getListOfPointsInRectangle (Position x y) (truncate length) (truncate width)))
        (getListOfPointsInCircle (Position cx cy) radius (getListOfPointsInRectangle (Position (cx-radius) (cy-radius)) (truncate (2*radius)) (truncate (2*radius)) )))

getOtherEndPoint :: Point -> Integer -> Float -> Point
getOtherEndPoint (Position x y) length theta = 
    (Position (x + ((fromIntegral length)*(cos theta))) (y - ((fromIntegral length)*(sin theta))))
   

   -- TODO old version by sukrut(Position (x + (cosComponent length theta)) (y + (sinComponent length theta)))

checkLineIfObstacle :: Point -> Point -> Integer -> Float -> Integer -> [[Tile]] -> Bool
checkLineIfObstacle (Position x y) (Position ox oy) i theta length tileMap =  
    ---- trace ("i is : " ++ show i ++ " x is : " ++ show x ++ " y is : " ++ show y ++ " ox is : " ++ show ox ++ "oy is : " ++ show oy ++ "\n") 
    (if (i < length) -- or <=?
        then if not (getIsObstacle tileMap (y - ((fromIntegral i) * sin(theta))) (x + ((fromIntegral i) * cos(theta))))
                then checkLineIfObstacle (Position x y) (Position ox oy) (i + 1) theta length tileMap
                else True
        else False)

-- returns true if line segment contains obstacle
checkLineSegmentObstacle :: Point -> Integer -> Float -> [[Tile]] -> Bool
checkLineSegmentObstacle (Position x y) length theta tileMap = 
    checkLineIfObstacle (Position x y) (getOtherEndPoint (Position x y) length theta) 1 theta length tileMap

thetaIncrement :: Float
thetaIncrement = 0.1

thetaMax :: Float
thetaMax = 1.57

searchForAngle :: Point -> Integer -> Float -> Float ->  [[Tile]] -> Float
searchForAngle (Position x y) length theta thetaMax tileMap = 
    ---- trace("Theta is search for angle : " ++ show theta ++ "\n")
    (if theta < thetaMax
        then if not (checkLineSegmentObstacle (Position x y) length theta tileMap)
                then theta
                else searchForAngle (Position x y) length (theta + thetaIncrement) thetaMax tileMap
        else (-1.0))

-- Accepts left end point of line and length of tank, and returns angle of its inclination
getAngleAt :: Point -> Integer ->  [[Tile]]  -> Float
getAngleAt (Position x y) length tileMap = searchForAngle (Position x y) length (0) thetaMax tileMap

{-
-- Accepts the centre and radius of a circle, tile map, and checks if it contains any obstacle in it or not
checkObstacleInCircle :: Point -> Float ->  [[Tile]] -> Bool
checkObstacleInCircle (Position cx cy) radius tileMap = checkObstacleInList (getListOfPointsInCircle (Position cx cy) radius 
    (getListOfPointsInRectangle (Position cx cy) (truncate (2*radius)) (truncate (2*radius)))) tileMap

-- Accept a list of points and tile map and returns if any of them contain an obstacle or not
checkObstacleInList :: [Point] -> [[Tile]] -> Bool
checkObstacleInList [] tileMap = False
checkObstacleInList (x:xs) tileMap = (getIsObstacle tileMap (truncate $ getPositionX x) (truncate $ getPositionY x))
    || (checkObstacleInList xs tileMap)
-}
radianTodegree::Float -> Float
radianTodegree x = (x*180)/pi

edgeOfTriangle :: Float
edgeOfTriangle = ((fromIntegral widthOfTank)*widthOfTile)/1.5

rectHalfAngle :: Float
rectHalfAngle = atan (2*(fromIntegral heightOfTank)/(fromIntegral widthOfTank))

hypotenuseRect :: Float
hypotenuseRect = sqrt((((fromIntegral heightOfTank)*heightOfTile)^2) + ((((fromIntegral widthOfTank)*widthOfTile)/2)^2))

makeTileNotObsAtPts :: [[Tile]] -> [Point] -> [[Tile]]
makeTileNotObsAtPts tileMatrix points = foldr (\(Position x y) tileMap -> changeListElementAtIndex tileMap (truncate y) (changeListElementAtIndex (tileMap !! (truncate y)) (truncate x) (Tile{isObstacle=False,tilePosition=getTilePos tileMap y x})  )  ) tileMatrix points

changeListElementAtIndex originalList index newValue = (take index originalList) ++ ( newValue : (drop (index+1) originalList))

getAllPointsInLine :: Point -> Integer -> Float -> Integer -> [Point]
getAllPointsInLine (Position x y) length theta i
	|	i<=length = (Position x y) : getAllPointsInLine (Position (x + (fromIntegral length) * (cos theta)) (y - (fromIntegral length) * (sin theta))) length theta (i+1)
	|	otherwise = []
