module Gamestate where

import qualified Graphics.UI.GLUT
import Types
import Tank
import Weapon
import Tile


getTankList :: [(Float , Float , Integer , Graphics.UI.GLUT.Color4 Float , Int , [Integer])] -> [Tank]
getTankList initialPosition = [(initializeTank a b c d e f) | (a,b,c,d,e,f) <- initialPosition] 

getWeaponList :: [(Float,Float,Float,Float,Float,Graphics.UI.GLUT.Color4 Float,Graphics.UI.GLUT.Color4 Float,Graphics.UI.GLUT.Vector3 Float,Float,Float)] -> [WeaponGraphics]
getWeaponList initialWeaponPosition = [(initializeWeapon a b c d e f g h i j) | (a,b,c,d,e,f,g,h,i,j) <- initialWeaponPosition]

initializeGamestate::GameState
initializeGamestate = GameState { tileMatrix = getTileMatrix, 
-- position x , y , score ,color , currentWeapon , weaponCount , 
                                  tankList = (getTankList [(10 , 150, 30 , Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , 0 , [10,10,10]),
                                                           (20 , 150, 30 , Graphics.UI.GLUT.Color4 0.8 0.4 0.6 1 , 0 , [1,1,1])--,
                                                          -- (62 , 150 , 30 , Graphics.UI.GLUT.Color4 0.123 0.03 0.24 1 , 0 , [10,10,10])
                                                          ]
                                               ),
-- position x y , factor , currentAngle , impactRadius , color , color , rotation , thickness length
                                  weapon = (getWeaponList [(0 , 0 , 2 , 1 , 30 , Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , Graphics.UI.GLUT.Color4 0.34 0.34 0.1686 1 , Graphics.UI.GLUT.Vector3 0 0.5 0.1 , 5  ,((fromIntegral heightOfTank)*heightOfTile)),
                                                           (0 , 0 , 3 , 1 , 20 , Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , Graphics.UI.GLUT.Color4 0.34 0.1686 0.34 1 , Graphics.UI.GLUT.Vector3 0.5 0.1 0 , 10 ,((fromIntegral heightOfTank)*heightOfTile)*1.5),
                                                           (0 , 0 , 4 , 1 , 10 , Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , Graphics.UI.GLUT.Color4 0.1686 0.34 0.34 1 , Graphics.UI.GLUT.Vector3 0.1 0.5 0 , 25 ,((fromIntegral heightOfTank)*heightOfTile)*2)
                                                           ]
                                             ),
                                  chance = 1,
                                  isAcceptingInput = True
                                }
