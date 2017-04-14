module Gamestate where

import qualified Graphics.UI.GLUT
import Types
import Tank
import Weapon
import System.IO.Unsafe
import qualified Graphics.Gloss
--import Tile


getTankList initialPosition = [(initializeTank a b c d e f) | (a,b,c,d,e,f) <- initialPosition] 

getWeaponList initialWeaponPosition = [(initializeWeapon a b c d e f g h i j) | (a,b,c,d,e,f,g,h,i,j) <- initialWeaponPosition]

initializeGamestate::GameState
initializeGamestate = GameState { tileMatrix = getTileMatrix, 
-- position x , y , score ,color , currentWeapon , weaponCount ,
                                  tankList = (getTankList [(10 , 149, 30 , Graphics.Gloss.green , 0 , [10,10,10]),
                                                           (30 , 149, 30 , Graphics.Gloss.blue , 0 , [10,10,10])--,
                                                        --   (67 , 149 , 30 , Graphics.Gloss.magenta , 0 , [10,10,10])
                                                          ]
                                               ),
-- position x y , factor , currentAngle , impactRadius , bullcolor , turrcolor , rotation , thickness length
                                  weapon = (getWeaponList [(0 , 0 , 2 , 1 , 15 , Graphics.Gloss.red , Graphics.Gloss.rose , 0 , 5  , (fromIntegral heightOfTank)* heightOfTile),
                                                           (0 , 0 , 3 , 2 , 10 , Graphics.Gloss.yellow , Graphics.Gloss.rose , 0 , 10 , (fromIntegral heightOfTank)*heightOfTile*1.5),
                                                           (0 , 0 , 4 , 3 , 5 , Graphics.Gloss.black , Graphics.Gloss.rose ,  0 , 25 , (fromIntegral heightOfTank)*heightOfTile*2)
                                                           ]
                                             ),
                                  chance = 1,
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
  where tile = ( startlist ++ [Tile{tilePosition = (Types.Position ((read ((contents!!j)) :: Float)/200) ((read (contents!!(j + 1)) :: Float)/100)), isObstacle = (tf (contents!!(j+2)) ) } ])

  --readcolumn 

tf :: String -> Bool
tf value 
  | value == "0" = False
  | value == "1" = True
  | otherwise = False


