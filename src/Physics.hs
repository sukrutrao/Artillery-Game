module Physics where

import Types
import Debug.Trace

-- | Acceleration due to gravity
g :: Float
g = 10

-- | Acceleration due to gravity in two dimensions
gAcceleration :: Point
gAcceleration = (Acceleration 0 g)

-- | A single unit of time for motion
unitTime :: Float
unitTime = 0.5

-- | To denote zero velocity in two dimensions
restVelocity :: Point
restVelocity = (Velocity 0 0)

-- | Function to accept two floats and return a point corresponding to them
originPosition :: Float --^ The x coordinate
               -> Float --^ The y coordinate
               -> Point --^ The equivalent point
originPosition x y = (Position x y)

-- | Function to obtain the new position in two dimensions given the position,
--   velocity, acceleration, and time based on Newton's second kinematic equation
newPosition :: Point -> Point -> Point -> Float -> Point
newPosition (Position x y) (Velocity vx vy) (Acceleration ax ay) time = 
	let temp = (Position (newOneDPosition x vx ax time) (newOneDPosition y (-vy) ay time))
    in if((truncate $ getPositionX temp) < 0)
            then Position 0 y
        	else if((truncate $ getPositionX temp) >= tileMatrixColumnSize)
          		  	then Position (fromIntegral (tileMatrixColumnSize-1)) y
             		else temp

-- | Function to obtain the new velocity in two dimensions given the velocity,
--   acceleration, and time based on Newton's first kinematic equation
newVelocity :: Point -> Point -> Float -> Point
newVelocity (Velocity vx vy) (Acceleration ax ay) time = 
   -- -- trace("vx in physics : " ++ show vx ++ "vy in physics : " ++ show vy ++ "\n")
    (Velocity (newOneDVelocity vx ax time) (newOneDVelocity (-vy) ay time))

-- | Function to obtain the new position in one dimension given the position,
--   velocity, acceleration, and time based on Newton's second kinematic equation
newOneDPosition :: Float -> Float -> Float -> Float -> Float
newOneDPosition x v a time = x + v * time + 0.5 * a * time * time

-- | Function to obtain the new velocity in one dimenstion given the velocity,
--   acceleration, and time based on Newton's first kinematic equation
newOneDVelocity :: Float -> Float -> Float -> Float
newOneDVelocity v a time = v + a * time

-- | Function to give the cosine component of a given quantity with respect to an angle theta
cosComponent :: Float -> Float -> Float
cosComponent quantity theta = quantity * cos(theta)

-- | Function to give the sine component of a given quantity with respect to an angle theta
sinComponent :: Float -> Float -> Float
sinComponent quantity theta = quantity * sin(theta)

-- | Function to return the components of the velocity in two dimensions given angle and magnitude
getComponentsVelocity :: Float -> Float -> Point
getComponentsVelocity quantity theta = (Velocity (cosComponent quantity theta) (sinComponent quantity theta))

-- | Function to accept a two dimensional position and velocity, and time, and return the 
--   new position under the influence of gravity
gravityNewPosition :: Point -> Point -> Float -> Point
gravityNewPosition (Position x y) (Velocity vx vy) time =
     trace("vx component : " ++ show vx ++ " vy component : " ++ show vy ++ "\n")
    newPosition (Position x y) (Velocity vx vy) gAcceleration time

-- | Function to return the new position in two dimensions given the position,
--   magnitude of velocity, angle, and acceleration in the first dimenstion,
--   with the acceleration in the other direction taken to be that of gravity
--   and time taken to be the unit time
newPositionVTheta :: Point -> Point -> Float -> Float -> Point
newPositionVTheta position velocity acceleration theta = newPosition position velocity (Acceleration acceleration g) unitTime

-- | Function to return the new position in two dimensions given position and time
--   under gravity when starting from rest
gravityNewPositionFromRest :: Point -> Float -> Point
gravityNewPositionFromRest (Position x y) time = gravityNewPosition (Position x y) restVelocity time

-- | Function to return the new position given the position, time, magnitude of
--   velocity and angle under gravity
getNewPositionUnderGravity :: Point -> Float -> Float -> Float -> Point
getNewPositionUnderGravity (Position x y) velocity theta time =
    gravityNewPosition (Position x y) (getComponentsVelocity velocity theta) time
    
-- | Function to return position which would be after unit time
newPositionGravityFrame :: Point -> Float -> Float -> Point
newPositionGravityFrame (Position x y) velocity theta =
    getNewPositionUnderGravity (Position x y) velocity theta unitTime

-- | Function to return the new position given position, magnitude of velocity
--   and angle in unit time under no acceleration
constantVelocityNewPosition :: Point -> Float -> Float ->  Point
constantVelocityNewPosition position velocity theta = newPosition position (getComponentsVelocity velocity theta) (Acceleration 0 0) unitTime

-- | Function to accept tile matrix, row, column and return if the corresponding
--   tile has an obstacle or not
getIsObstacle:: [[Tile]] -> Float -> Float -> Bool
getIsObstacle tileMatrix row col 
    | ((col < 0 || col >= (fromIntegral tileMatrixColumnSize) || row >= (fromIntegral tileMatrixRowSize))) = (True)
    | (row < 0) = (False)
    | otherwise = (isObstacle ((tileMatrix !! (truncate row)) !! (truncate col)))

-- | Function to return the point corresponding to a given row and column of a tile
--   matrix after checking for validity
getTilePos:: [[Tile]] -> Float -> Float -> Point
getTilePos tileMatrix row col 
    | (row < 0 && col < 0) = tilePosition ((tileMatrix !! 0) !! 0)
    | (row < 0 && col > matrColSize) = tilePosition ((tileMatrix !! 0) !! truncatedmatrColSize)
    | (row > matrRowSize && col < 0 ) = tilePosition ((tileMatrix !! truncatedmatrRowSize) !! 0)
    | (row > matrRowSize && col > matrColSize) = tilePosition ((tileMatrix !! truncatedmatrRowSize) !! truncatedmatrColSize)
    | (row > matrRowSize) = (tilePosition ((tileMatrix !! truncatedmatrRowSize) !! truncatedCol))
    | (col > matrColSize) = tilePosition ((tileMatrix !! truncatedRow) !! truncatedmatrColSize)
    | (col < 0) = tilePosition ((tileMatrix !! truncatedRow) !! 0) 
    | otherwise = tilePosition ((tileMatrix !! truncatedRow) !! truncatedCol)
    where matrColSize = (fromIntegral tileMatrixColumnSize)-1
          truncatedmatrColSize = truncate matrColSize
          matrRowSize = (fromIntegral tileMatrixRowSize)-1
          truncatedmatrRowSize = truncate matrRowSize
          truncatedCol = truncate col
          truncatedRow = truncate row

-- | Function to return the x coordinate of the point corresponding to a given row and column of a tile
--   matrix after checking for validity
getTilePosX:: [[Tile]] -> Float -> Float -> Float
getTilePosX tileMatrix row col 
    | (row < 0 && col < 0) = getPositionX $ tilePosition ((tileMatrix !! 0) !! 0)
    | (row < 0 && col > matrColSize) = getPositionX $ tilePosition ((tileMatrix !! 0) !! truncatedmatrColSize)
    | (row > matrRowSize && col < 0 ) = getPositionX $ tilePosition ((tileMatrix !! truncatedmatrRowSize) !! 0)
    | (row > matrRowSize && col > matrColSize) = getPositionX $ tilePosition ((tileMatrix !! truncatedmatrRowSize) !! truncatedmatrColSize)
    | (row > matrRowSize) = getPositionX $ tilePosition ((tileMatrix !! truncatedmatrRowSize) !! truncatedCol)
    | (col > matrColSize) = getPositionX $ tilePosition ((tileMatrix !! truncatedRow) !! truncatedmatrColSize)
    | (col < 0) = getPositionX $ tilePosition ((tileMatrix !! truncatedRow) !! 0) 
    | otherwise = getPositionX $ tilePosition ((tileMatrix !! truncatedRow) !! truncatedCol)
    where matrColSize = (fromIntegral tileMatrixColumnSize)-1
          truncatedmatrColSize = truncate matrColSize
          matrRowSize = (fromIntegral tileMatrixRowSize)-1
          truncatedmatrRowSize = truncate matrRowSize
          truncatedCol = truncate col
          truncatedRow = truncate row

-- | Function to return the y coordinate of the point corresponding to a given row and column of a tile
--   matrix after checking for validity
getTilePosY:: [[Tile]] -> Float -> Float -> Float
getTilePosY tileMatrix row col 
    | (row < 0 && col < 0) = getPositionY $ tilePosition ((tileMatrix !! 0) !! 0)
    | (row < 0 && col > matrColSize) = getPositionY $ tilePosition ((tileMatrix !! 0) !! truncatedmatrColSize)
    | (row > matrRowSize && col < 0 ) = getPositionY $ tilePosition ((tileMatrix !! truncatedmatrRowSize) !! 0)
    | (row > matrRowSize && col > matrColSize) = getPositionY $ tilePosition ((tileMatrix !! truncatedmatrRowSize) !! truncatedmatrColSize)
    | (row > matrRowSize) = getPositionY $ tilePosition ((tileMatrix !! truncatedmatrRowSize) !! truncatedCol)
    | (col > matrColSize) = getPositionY $ tilePosition ((tileMatrix !! truncatedRow) !! truncatedmatrColSize)
    | (col < 0) = getPositionY $ tilePosition ((tileMatrix !! truncatedRow) !! 0) 
    | otherwise = getPositionY $ tilePosition ((tileMatrix !! truncatedRow) !! truncatedCol)
    where matrColSize = (fromIntegral tileMatrixColumnSize)-1
          truncatedmatrColSize = truncate matrColSize
          matrRowSize = (fromIntegral tileMatrixRowSize)-1
          truncatedmatrRowSize = truncate matrRowSize
          truncatedCol = truncate col
          truncatedRow = truncate row

-- | Function to return the x coordinate of a point
getPositionX:: Point -> Float
getPositionX (Position x _) = x

-- | Function to return the y coordinate of a point
getPositionY:: Point -> Float
getPositionY (Position _ y) = y

-- | Function to return the angle of a projectile given magnitude of velocity
--   and current angle for change in unit time
getAngleProjectile :: Float -> Float -> Direction -> Float 
getAngleProjectile velocity theta FacingRight = trace ("R+++++++++++++ theta: " ++ show theta ++ "  ,  velocity : " ++ show velocity ++ "\n") ((-1) * atan((-1) * tan(theta) + (g * unitTime)/(velocity * cos(theta))))
getAngleProjectile velocity theta FacingLeft = trace ("L+++++++++++++ theta: " ++ show theta ++ "  ,  velocity : " ++ show velocity ++ "\n") (pi  + (-1) * atan((-1) * tan(theta) + (g * unitTime)/(velocity * cos(theta))))

-- | Function to return the new position of a projectile given position, magnitude of velocity
--   and current angle for change in unit time
getPositionProjectile :: Point -> Float -> Float -> Point 
getPositionProjectile position velocity theta = getNewPositionUnderGravity position velocity theta unitTime

-- | Function to return the magnitude of velocity of a projectile given magnitude of velocity
--   and current angle for change in unit time
getVelocityProjectile :: Float -> Float -> Float
getVelocityProjectile velocity theta = sqrt((velocity * cos(theta))^2 + ((-1) * velocity * sin(theta) + g * unitTime)^2)

-- | enum to denote if a point is above or below a line
data PointLineOrientation = AboveLine | BelowLine deriving(Enum, Eq)

-- | Function to accept two points and return its slope and intercept
getLineSlopeIntercept :: Point -> Point -> (Float,Float)
getLineSlopeIntercept (Position x1 y1) (Position x2 y2) = (((y2-y1)/(x2-x1)), y1-(x1*((y2-y1)/(x2-x1))))

-- | Function to accept a point, and two end points of a line, and check the points orientation
--   with respect to the line
checkOrientationPointLine :: Point -> Point -> Point -> PointLineOrientation
checkOrientationPointLine (Position x y) first second
    |    y < x*fst(getLineSlopeIntercept first second) + snd(getLineSlopeIntercept first second) = AboveLine
    |    otherwise = BelowLine -- for multiples of 90 degrees?
    
-- | Function to accept a point, the bottom left vertex of a rectangle, length, width,
--   angle of inclination and return whether the point lies in the rectangle or not
checkPointInRectangle :: Point -> Point -> Integer -> Integer -> Float -> Bool
checkPointInRectangle point (Position lx ly) ilength iwidth theta = 
    let length = fromIntegral ilength
        width = fromIntegral iwidth
        pointX = getPositionX point
        pointY = getPositionY point
    in
	   -- trace("CPIR : " ++ show point ++ " " ++ show lx ++ " " ++ show ly ++ " " ++ show ilength ++ " " ++ show iwidth ++ " " ++ show theta ++ " " ++ show (not (theta == 0)))
	    (if (not (theta == 0))
	    	then if ((checkOrientationPointLine point (Position lx ly) (Position (lx + length * cos(theta)) (ly - length * sin(theta)))) == AboveLine &&
			        (checkOrientationPointLine point (Position lx ly) (Position (lx - width * sin(theta)) (ly - width * cos(theta)))) == AboveLine &&
			        (checkOrientationPointLine point (Position (lx - width * sin(theta) + length * cos(theta)) (ly - width * cos(theta) - length * sin(theta))) (Position (lx + length * cos(theta)) (ly - length * sin(theta)))) == BelowLine &&
			        (checkOrientationPointLine point (Position (lx - width * sin(theta) + length * cos(theta)) (ly - width * cos(theta) - length * sin(theta))) (Position (lx - width * sin(theta)) (ly - width * cos(theta)))) == BelowLine)
			        then True
			        else False
			else if (pointX >= lx && pointX <= lx + length && pointY <= ly && pointY >= ly - width)
				then True
				else False)
        
-- | Function to accept a point, the centre of a circle, and radius, and return if
--   the point lies in the circle or not
checkPointInCircle :: Point -> Point -> Float -> Bool
checkPointInCircle (Position x y) (Position cx cy) radius
    |    (x-cx)^2 + (y-cy)^2 <= radius^2 = True -- trace("Tx,y,cx,cy,radius : " ++ show x ++ show y ++ show cx ++ show cy ++ show radius ++ "\n") True
    |    otherwise = False -- trace("Fx,y,cx,cy,radius : " ++ show x ++ show y ++ show cx ++ show cy ++ show radius ++ "\n") 
    
-- | Function to accept the center and radius of a circle, and the bounding
--   square of the circle and return the list of points in it
getListOfPointsInCircle :: Point -> Float -> [[Point]] -> [Point]
getListOfPointsInCircle (Position cx cy) radius [] = []
getListOfPointsInCircle (Position cx cy) radius [[]] = []
getListOfPointsInCircle (Position cx cy) radius (x:xs) =  -- trace(show xs)
    ((checkCommonPointsCircleLine (Position cx cy) radius x) ++ (getListOfPointsInCircle (Position cx cy) radius xs))

-- | Function to accept the center and radius of a circle and return
--   the list of points in it 
getAllPointsInCircle :: Point -> Float -> [Point]
getAllPointsInCircle (Position cx cy) radius = 
	getListOfPointsInCircle (Position cx cy) radius $ getListOfPointsInRectangle (Position (cx - radius) (cy - radius)) (truncate (2*radius)) (truncate (2*radius)) 

-- | Function to accept the centre and radius of a circle and a list of points
--   and return a list of common points
checkCommonPointsCircleLine :: Point -> Float -> [Point] -> [Point]
checkCommonPointsCircleLine (Position cx cy) radius [] = []
checkCommonPointsCircleLine (Position cx cy) radius (x:xs) = 
    if (checkPointInCircle x (Position cx cy) radius) then (x : (checkCommonPointsCircleLine (Position cx cy) radius xs))
    else (checkCommonPointsCircleLine (Position cx cy) radius xs)
        
-- | Function to accept the left vertex and length of a horizontal line and return
--   the list of points in it
getListOfPointsInLine :: Point -> Integer -> [Point]
getListOfPointsInLine (Position x y) length
    |    length < 0 = []
    |    otherwise = ((Position x y) : (getListOfPointsInLine (Position (x+1) y) (length - 1)))
    
-- | Function to accept the bottom left vertex, length, and width of a horizontal
--   rectangle and return a list of lists of points in it
getListOfPointsInRectangle :: Point -> Integer -> Integer -> [[Point]]
getListOfPointsInRectangle (Position x y) length width
    |    width < 0 = [[]]
    |    otherwise = ((getListOfPointsInLine (Position x y) length) : (getListOfPointsInRectangle (Position x (y+1)) length (width-1)))
    
-- | Function to accept a two dimensional list and returna one dimensional list
flattenList [] = []
flattenList [[]] = []
flattenList (x:xs) = (x ++ (flattenList xs))
    
-- | Function to accept two lists of Integer tuples and return a list of
--   common points
commonPointsBetweenLists :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
commonPointsBetweenLists [] [] = [] 
commonPointsBetweenLists (x:xs) [] = []
commonPointsBetweenLists [] y = []
commonPointsBetweenLists (x:xs) y = if (x `elem` y) then (x : (commonPointsBetweenLists xs y))
                                    else (commonPointsBetweenLists xs y)

-- | Function to accept a list of points and return a corresponding list
--   of Integer tuples
convertPointListToInteger :: [Point] -> [(Integer,Integer)]
convertPointListToInteger [] = []
convertPointListToInteger (x:xs) = (((truncate $ getPositionX x), (truncate $ getPositionY x)) : (convertPointListToInteger xs))

--| Function to return a list of common points between a circle and a rectangle                              
commonPointsBetweenCircleRectangle :: Point -> Float -> Point -> Float -> Float -> [(Integer,Integer)]
commonPointsBetweenCircleRectangle (Position cx cy) radius (Position x y) length width = 
    (commonPointsBetweenLists (convertPointListToInteger(flattenList $ (getListOfPointsInRectangle (Position x y) (truncate length) (truncate width))))
        (convertPointListToInteger(getListOfPointsInCircle (Position cx cy) radius (getListOfPointsInRectangle (Position (cx-radius) (cy-radius)) (truncate (2*radius)) (truncate (2*radius)) ))))

-- Function to return the other end point of a given line
getOtherEndPoint :: Point -> Integer -> Float -> Point
getOtherEndPoint (Position x y) length theta = 
    (Position (x + ((fromIntegral length)*(cos theta))) (y - ((fromIntegral length)*(sin theta))))
   
-- | Function to help check if a given line contains a point with an obstacle in it
checkLineIfObstacle :: Point -> Point -> Integer -> Float -> Integer -> [[Tile]] -> Bool
checkLineIfObstacle (Position x y) (Position ox oy) i theta length tileMap =  
    ---- trace ("i is : " ++ show i ++ " x is : " ++ show x ++ " y is : " ++ show y ++ " ox is : " ++ show ox ++ "oy is : " ++ show oy ++ "\n") 
    (if (i < length)
        then if not $ isIndexInRange tileMap (truncate (y - ((fromIntegral i) * sin(theta))))
                    then True
             else let xTemp = (x +((fromIntegral i) * cos(theta))) 
                  in if not $ getIsObstacle tileMap (y - ((fromIntegral i) * sin(theta))) (if(truncate xTemp >= tileMatrixColumnSize) then (fromIntegral tileMatrixColumnSize) - ((fromIntegral widthOfTank)*3) else xTemp)
                        then checkLineIfObstacle (Position x y) (Position ox oy) (i + 1) theta length tileMap
                        else True
        else False)

-- | Function to check if a given line contains a point with an obstacle in it
checkLineSegmentObstacle :: Point -> Integer -> Float -> [[Tile]] -> Bool
checkLineSegmentObstacle (Position x y) length theta tileMap = 
    checkLineIfObstacle (Position x y) (getOtherEndPoint (Position x y) length theta) 1 theta length tileMap

-- | The increment of theta for finding the angle of inclination of
--   the tank
thetaIncrement :: Float
thetaIncrement = 0.1

-- | The maximum possible angle
thetaMax :: Float
thetaMax = pi/2

-- | Function to find the minimum valid angle of inclination of
--   a given rectangle
searchForAngle :: Point -> Integer -> Integer -> Float -> Float ->  [[Tile]] -> Float
searchForAngle (Position x y) length width theta thetaMax tileMap = 
    ---- trace("Theta is search for angle : " ++ show theta ++ "\n")
    (if theta < thetaMax
        then if not (checkIfNotValidPosition (Position x y) length width theta tileMap)
                then (theta+thetaIncrement)
                else searchForAngle (Position x y) length width (theta + thetaIncrement) thetaMax tileMap
        else (pi))

-- | Function that accepts left end point of line and length of tank, and returns
--   angle of its inclination
getAngleAt :: Point -> Integer ->  Integer -> [[Tile]]  -> Float
getAngleAt (Position x y) length width tileMap = searchForAngle (Position x y) length width (-pi/2) thetaMax tileMap


-- | Function that accepts the centre and radius of a circle, tile map, and checks
--   if it contains any obstacle in it or not
checkObstacleInCircle :: Point -> Float ->  [[Tile]] -> Bool
checkObstacleInCircle (Position cx cy) radius tileMap = checkObstacleInList (getListOfPointsInCircle (Position cx cy) radius 
    (getListOfPointsInRectangle (Position cx cy) (truncate (2*radius)) (truncate (2*radius)))) tileMap

-- | Function that accepts a list of points and tile map and returns if any
--   of them contain an obstacle or not
checkObstacleInList :: [Point] -> [[Tile]] -> Bool
checkObstacleInList [] tileMap = False
checkObstacleInList (x:xs) tileMap = (getIsObstacle tileMap (getPositionY x) (getPositionX x))
    || (checkObstacleInList xs tileMap)

-- | Function to convert an angle from radians to degrees
radianTodegree::Float -> Float
radianTodegree x = (x*180)/pi

-- | To fix the edge length of the triangle marker
edgeOfTriangle :: Float
edgeOfTriangle = ((fromIntegral widthOfTank)*widthOfTile)/1.5

-- | The angle of inclination of the top mid point of a rectangle with respect
--   to the bottom left vertex
rectHalfAngle :: Float
rectHalfAngle = atan (2*(fromIntegral heightOfTank)/(fromIntegral widthOfTank))

-- | The distance between the top center point and the left bottom vertex
--   of a rectangle
hypotenuseRect :: Float
hypotenuseRect = sqrt((((fromIntegral heightOfTank)*heightOfTile)^2) + ((((fromIntegral widthOfTank)*widthOfTile)/2)^2))

-- | Function to accept a list of points and the tile matrix and remove the obstacle
--   property at those points
makeTileNotObsAtPts :: [[Tile]] -> [Point] -> [[Tile]]
makeTileNotObsAtPts tileMatrix points = (foldr (\(Position x y) tileMap -> changeListElementAtIndex tileMap (truncate y) (changeListElementAtIndex (tileMap !! (truncate y)) (truncate x) (Tile{isObstacle=False,tilePosition=getTilePos tileMap y x})  )  ) tileMatrix points)

-- | Function to change the value of an element of a list at a given index
changeListElementAtIndex originalList index newValue = (take index originalList) ++ ( newValue : (drop (index+1) originalList))

-- | Function to return a list of all points in a line
getAllPointsInLine :: Point -> Integer -> Float -> Integer -> [Point]
getAllPointsInLine (Position x y) length theta i
	|	i<=length = (Position (x + (fromIntegral i) * (cos theta)) (y - (fromIntegral i) * (sin theta))) : getAllPointsInLine (Position x y) length theta (i+1)
	|	otherwise = []

-- | Function to help get a list of all points in a given rectangle
getAllPointsInRectangleHelper :: Point -> Integer -> Integer -> Float -> Integer -> [[Point]]
getAllPointsInRectangleHelper (Position x y) length width theta i
	|	i<=width = (getAllPointsInLine (Position (x - (fromIntegral i) * (sin theta)) (y - (fromIntegral i) * (cos theta))) length theta 0) : (getAllPointsInRectangleHelper (Position x y) length width theta (i+1))
	|	otherwise = [[]] 

-- | Function to return the list of all points in a given rectangle
getAllPointsInRectangle :: Point -> Integer -> Integer -> Float -> [Point]
getAllPointsInRectangle (Position x y) length width theta = 
	flattenList $ getAllPointsInRectangleHelper (Position x y) length width theta 0

-- | Function that checks whether a given position and orientation of a rectangle
--   is free from obstacles or not
checkIfNotValidPosition :: Point -> Integer -> Integer -> Float -> [[Tile]] -> Bool
checkIfNotValidPosition (Position x y) length width theta tileMap = 
	checkObstacleInList (getAllPointsInRectangle (Position x y) length width theta) tileMap

-- | Function to return the new position of a tank under the influence of gravity
tankGravityNewPosition :: Point -> Integer -> Integer -> Float -> [[Tile]] -> Point
tankGravityNewPosition (Position x y) length width theta tileMap
	|	checkIfNotValidPosition (Position x (y+1)) length width theta tileMap == False = 
			tankGravityNewPosition (Position x (y+1)) length width theta tileMap
	|	otherwise = (Position x y)

-- | Function to calculate the y coordinate of a given x coordinate value
--   for an object in projectile motion
parabolaFunction :: Point -> Float -> Float -> Float -> Float
parabolaFunction (Position sx sy) x velocity theta = 
	if abs(theta - pi/2) > 0.1
		then {-trace ("PARABOLA+++++++ SX : " ++ show sx  ++ " , SY : " ++ show sy ++ " velocity : " ++ show velocity ++ " theta : " ++ show theta ++ " X : " ++  show x ++ " , Y : " ++ show (sy - (x - sx) * (tan theta) + (0.5 * g * (x - sx)^2)/((velocity * (cos theta))^2)))-}
	(sy - (x - sx) * (tan theta) + (0.5 * g * (x - sx)^2)/((velocity * (cos theta))^2))
		else trace("THIS") (0)

--| Function to check if the path between two points has any obstacle in it
--  so that weapons don't overshoot tanks or mountains in a unit time
checkIntermediateObstacleInPath :: GameState -> Point -> Point -> Point -> Float -> Float -> [[Tile]] -> Bool -> Point
checkIntermediateObstacleInPath gameState (Position x y) (Position ox oy) (Position sx sy) velocity theta tileMap xIsLesser =
    let newTheta = theta {-if xIsLesser then atan(sqrt((tan theta)^2 + (2*g)*(velocity*(cos theta))))
    							else pi + atan(sqrt((tan theta)^2 + (2*g)*(velocity*(cos theta))))-}
        newVelocity = velocity {-sqrt(velocity^2 + 2*g)-}
        currentYP = (parabolaFunction (Position sx sy) (x+1) newVelocity newTheta)
        currentYN = (parabolaFunction (Position sx sy) (x-1) newVelocity newTheta) in
    if (xIsLesser && x < ox && abs(theta - pi/2) > 0.1)
        then (if getIsObstacle tileMap currentYP (x+1) || checkAllTanksForHit gameState (Position (x+1) currentYP)
                then (Position (x+1) (parabolaFunction (Position sx sy) (x+1) newVelocity newTheta))
                else checkIntermediateObstacleInPath gameState (Position (x+1) (parabolaFunction (Position sx sy) (x+1) newVelocity newTheta))
                      (Position ox oy) (Position sx sy) newVelocity newTheta tileMap xIsLesser)		
        else (if ((not xIsLesser) && ox < x && abs(theta - pi/2) > 0.1)
                then (if getIsObstacle tileMap currentYN (x-1) || checkAllTanksForHit gameState (Position (x-1) currentYN)
                        then (Position (x-1) (parabolaFunction (Position sx sy) (x-1) newVelocity newTheta))
                        else checkIntermediateObstacleInPath gameState (Position (x-1) (parabolaFunction (Position sx sy) (x-1) newVelocity newTheta))
                              (Position ox oy) (Position sx sy) newVelocity newTheta tileMap xIsLesser)
                else if (abs(theta - pi/2) < 0.1)
                		then trace("GOING HERE!!!") (getPointFromYChecks gameState x y oy (y<oy) tileMap)
                		else trace("GOING THERE!!!") (Position ox oy))

-- | Function to check the path for obstacles when the object is moving in
--   a vertical line, where the parabola function fails as theta will be pi/2
getPointFromYChecks :: GameState -> Float -> Float -> Float -> Bool -> [[Tile]] -> Point
getPointFromYChecks gameState x y oy yIsLesser tileMap = 
	if (yIsLesser && y<oy)
		then if (getIsObstacle tileMap (y+1) x || checkAllTanksForHit gameState (Position x (y+1)))
				then (Position x (y+1))
				else getPointFromYChecks gameState x (y+1) oy yIsLesser tileMap
		else if ((not yIsLesser) && oy<y)
				then if (getIsObstacle tileMap (y-1) x || checkAllTanksForHit gameState (Position x (y-1)))
						then (Position x (y-1))
						else getPointFromYChecks gameState x (y-1) oy yIsLesser tileMap
				else (Position x oy)


-- | Function to return the new position of a projectile taking into account
--   obstacles in its path
newPositionProjectile :: GameState -> Point -> Point -> Float -> Float -> Float -> Float -> [[Tile]] -> Point
newPositionProjectile gameState initialPosition position velocity theta lv lt tileMap = 
    let otherPosition = getPositionProjectile position velocity theta 
        xIsLesser = getPositionX position < getPositionX otherPosition in
    	checkIntermediateObstacleInPath gameState position otherPosition initialPosition lv lt tileMap xIsLesser

-- | Function to get the position of the tip of the turret from which
--   the weapon was shot
getTurretPosition :: GameState -> Float -> Point
getTurretPosition (GameState {
	    tileMatrix = tileMap,
	    tankList = tanks,
	    chance = c
	}) lTurr = 
    let incline_theta = (inclineAngle (tankState (tanks !! c)))
        x = (getPositionX (position (tankState (tanks !! c))))
        y = (getPositionY (position (tankState (tanks !! c))))
        turret_theta = angle (turret (tankState (tanks !! c)))
        topCenterX = x+((hypotenuseRect*cos(incline_theta+rectHalfAngle))/widthOfTile)
        topCenterY = y-((hypotenuseRect*sin(incline_theta+rectHalfAngle))/heightOfTile)
        turretTopX = topCenterX+((lTurr*cos(incline_theta+turret_theta))/heightOfTile)
        turretTopY = topCenterY-((lTurr*sin(incline_theta+turret_theta))/heightOfTile)
        in  (Position (turretTopX) (turretTopY))

-- | Function to help check if any tank has been hit by a weapon
checkAllTanksForHitHelper :: GameState -> Point -> Int -> Bool
checkAllTanksForHitHelper (GameState {
	    tankList = tanks,
        noOfPlayers = n
	}) (Position x y) i
	|	i < n = trace("CATFHH : " ++ show i) (checkPointInRectangle (Position x y) (position (tankState (tanks !! i))) widthOfTank
					heightOfTank (inclineAngle (tankState (tanks !! i))) ||
						checkAllTanksForHitHelper (GameState {
				   						 tankList = tanks,
									     noOfPlayers = n
									}) (Position x y) (i+1))
	|	otherwise = False

-- | Function to check if any tank has been hit by a weapon
checkAllTanksForHit :: GameState -> Point -> Bool
checkAllTanksForHit gameState position = trace("CATCH : " ++ show (checkAllTanksForHitHelper gameState position 0))
											(checkAllTanksForHitHelper gameState position 0)

-- | The minimum allowed angle for a tank
minValid :: Float
minValid = (-(4*pi)/5)

-- | The maximum allowed angle for a tank
maxValid :: Float
maxValid = ((4*pi)/5)

-- | Function to check if a given angle is in the valid range or not
checkThetaValidRange :: Float -> Bool
checkThetaValidRange theta = if (theta >= minValid && theta <= maxValid) then True else False