module Gamestate where

import qualified Graphics.UI.GLUT
import Types
import Tank
import Weapon
import System.IO.Unsafe
--import Tile


getTankList :: [(Float , Float , Float , Graphics.UI.GLUT.Color4 Float , Int , [Integer])] -> [Tank]
getTankList initialPosition = [(initializeTank a b c d e f) | (a,b,c,d,e,f) <- initialPosition] 

getWeaponList :: [(Float,Float,Float,Float,Float,Graphics.UI.GLUT.Color4 Float,Graphics.UI.GLUT.Color4 Float,Graphics.UI.GLUT.Vector3 Float,Float,Float)] -> [WeaponGraphics]
getWeaponList initialWeaponPosition = [(initializeWeapon a b c d e f g h i j) | (a,b,c,d,e,f,g,h,i,j) <- initialWeaponPosition]

initializeGamestate::GameState
initializeGamestate = GameState { tileMatrix = getTileMatrix, 
-- position x , y , score ,color , currentWeapon , weaponCount ,
                                  tankList = (getTankList [(10 , 129, 30 , Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , 0 , [10,10,10]),
                                                           (20 , 129, 30 , Graphics.UI.GLUT.Color4 0.8 0.4 0.6 1 , 0 , [10,10,10])--,
                                                          -- (30 , 149 , 30 , Graphics.UI.GLUT.Color4 0.123 0.03 0.24 1 , 0 , [10,10,10])
                                                          ]
                                               ),
-- position x y , factor , currentAngle , impactRadius , color , color , rotation , thickness length
                                  weapon = (getWeaponList [(0 , 0 , 2 , 1 , 15 , Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , Graphics.UI.GLUT.Color4 0.34 0.34 0.1686 1 , Graphics.UI.GLUT.Vector3 0 0.5 0.1 , 5  ,((fromIntegral heightOfTank)*heightOfTile)),
                                                           (0 , 0 , 3 , 2 , 10 , Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , Graphics.UI.GLUT.Color4 0.34 0.1686 0.34 1 , Graphics.UI.GLUT.Vector3 0.5 0.1 0 , 10 ,((fromIntegral heightOfTank)*heightOfTile)*1.5),
                                                           (0 , 0 , 4 , 3 , 5 , Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , Graphics.UI.GLUT.Color4 0.1686 0.34 0.34 1 , Graphics.UI.GLUT.Vector3 0.1 0.5 0 , 25 ,((fromIntegral heightOfTank)*heightOfTile)*2)
                                                           ]
                                             ),
                                  chance = 0,
                                  isAcceptingInput = True,
                                  noOfPlayers = 2
                                }




getTileMatrix :: [[Tile]]
getTileMatrix = (readrow contents i []) 
  where contents = ( lines str) 
        i = 0
        str = unsafePerformIO (readFile "Level1.txt")
 
readrow :: [String] -> Int -> [[Tile]] -> [[Tile]]
readrow contents i startlist
  | contents !! i == "$" = startlist
  | otherwise = ( (readrow contents (i+1) tiles) )
  where tiles = ( startlist ++ [(readcolumn ( words (contents!!i)) 0  [])] ) 

  
readcolumn :: [String] -> Int -> [Tile] -> [Tile]
readcolumn contents j startlist
  | contents!!j == "#" = startlist
  | otherwise = (readcolumn contents (j+3) tile )
  where tile = ( startlist ++ [Tile{tilePosition = (Types.Position ((read (contents!!j) :: Float)/200) ((read (contents!!(j + 1)) :: Float)/100)), isObstacle = (tf (contents!!(j+2)) ) } ])

  --readcolumn 

tf :: String -> Bool
tf value 
  | value == "0" = False
  | value == "1" = True
  | otherwise = False


