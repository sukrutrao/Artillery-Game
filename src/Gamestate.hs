module Gamestate where

import qualified Graphics.UI.GLUT
import Types
import Tank
import Weapon
import System.IO.Unsafe
--import Tile

-- | Function to accept tank parameters and call initializeTank in module Tank to initialize tank parameters
getTankList :: [(Float , Float , Float , Float , Graphics.UI.GLUT.Color4 Float , Int , [Integer])] -> [Tank]
getTankList initialPosition = [(initializeTank a b c d e f g) | (a,b,c,d,e,f,g) <- initialPosition] 

-- | Function to accept weapon parameters and call initializeWeapon in module Weapon to initialize weapon parameters
getWeaponList :: [(Float,Float,Float,Float,Float,Graphics.UI.GLUT.Color4 Float,Graphics.UI.GLUT.Color4 Float,Graphics.UI.GLUT.Vector3 Float,Float,Float)] -> [WeaponGraphics]
getWeaponList initialWeaponPosition = [(initializeWeapon a b c d e f g h i j) | (a,b,c,d,e,f,g,h,i,j) <- initialWeaponPosition]

-- | Function to give the initial values of the game by the user 
initializeGamestate::GameState
initializeGamestate = GameState { tileMatrix = getTileMatrix, 
-- position x , y , score , angle , color , currentWeapon , weaponCount ,
                                  tankList = (getTankList [(20 , 137, 30 , pi/4 , Graphics.UI.GLUT.Color4 0.8 0.4 0.6 1 , 0 , [1000,10,2]),
                                                           (120 , 129, 30 , pi/4 ,  Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , 0 , [1000,10,2]),
                                                           (275 , 130 , 30 , 0 ,  Graphics.UI.GLUT.Color4 0.123 0.03 0.24 1 , 0 , [1000,10,2])
                                                          ]
                                               ),
-- position x ,  y , factor , currentAngle , impactRadius , color , color , rotation , thickness length
                                  weapon = (getWeaponList [(0 , 0 , 2 , 1 , 7 , Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , Graphics.UI.GLUT.Color4 0.34 0.34 0.1686 1 , Graphics.UI.GLUT.Vector3 0 0.5 0.1 , 5  ,((fromIntegral heightOfTank)*heightOfTile)),
                                                           (0 , 0 , 5 , 2 , 4 , Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , Graphics.UI.GLUT.Color4 0.34 0.1686 0.34 1 , Graphics.UI.GLUT.Vector3 0.5 0.1 0 , 10 ,((fromIntegral heightOfTank)*heightOfTile)*1.5),
                                                           (0 , 0 , 10 , 3 , 2 , Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , Graphics.UI.GLUT.Color4 0.1686 0.34 0.34 1 , Graphics.UI.GLUT.Vector3 0.1 0.5 0 , 25 ,((fromIntegral heightOfTank)*heightOfTile)*2)
                                                           ]
                                             ),
                                  chance = 0,
                                  isAcceptingInput = True,
                                  noOfPlayers = 3
                                }



-- | Function to read tile map parameters from file and return a matrix of tiles 
getTileMatrix :: [[Tile]]
getTileMatrix = (readrow contents i []) 
  where contents = ( lines str) 
        i = 0
        str = unsafePerformIO (readFile "Level1.txt")

-- | SubFunction of getTileMatrix to be called for adding a new row in the tilematrix 
readrow :: [String] -> Int -> [[Tile]] -> [[Tile]]
readrow contents i startlist
  | contents !! i == "$" = startlist
  | otherwise = ( (readrow contents (i+1) tiles) )
  where tiles = ( startlist ++ [(readcolumn ( words (contents!!i)) 0  [])] ) 

-- | SubFunction of readrow to be called for adding a new column to a row in the tilematrix 
readcolumn :: [String] -> Int -> [Tile] -> [Tile]
readcolumn contents j startlist
  | contents!!j == "#" = startlist
  | otherwise = (readcolumn contents (j+3) tile )
  where tile = ( startlist ++ [Tile{tilePosition = (Types.Position ((read (contents!!j) :: Float)/200) ((read (contents!!(j + 1)) :: Float)/100)), isObstacle = (tf (contents!!(j+2)) ) } ])


-- | Function to convert Int To Bool For Tile isObstacle Parameter
tf :: String -> Bool
tf value 
  | value == "0" = False
  | value == "1" = True
  | otherwise = False


