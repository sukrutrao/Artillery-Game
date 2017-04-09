module Gamestate where

import qualified Graphics.UI.GLUT
import Types
import Tank
import Weapon

widthOfTile :: Float
widthOfTile = 1

heightOfTile :: Float
heightOfTile = 1

getTileMatrix :: [[Tile]] 
getTileMatrix = [[Tile {tileposition = (Position (-1) 0),isObstacle = False } , Tile {tileposition = (Position 0 0),isObstacle = False } ] ,
                 [Tile {tileposition = (Position (-1) (-1)),isObstacle = True } , Tile {tileposition = (Position 0 (-1)),isObstacle = True } ]
                ]

getTankList :: [(Float , Float , Graphics.UI.GLUT.Color4 Float , Point)] -> [Tank]
getTankList initialPosition = [(initializeTank x y z o) | (x,y,z,o) <- initialPosition] 

getWeaponList :: [(Float , Float)] -> [Weapon]
getWeaponList initialWeaponPosition = [(initializeWeapon x y) | (x,y) <- initialWeaponPosition]

initializeGamestate::GameState
initializeGamestate = GameState {tileMatrix = (getTileMatrix), tankList = (getTankList [((-0.75) , 0 , Graphics.UI.GLUT.Color4 0.5 0.5 0.1 1 , (Position (-0.9) 0.9)) , (0 , 0 , Graphics.UI.GLUT.Color4 0.8 0.4 0.6 1 , (Position 0.5 0.9))]), weapon = (getWeaponList [((-0.75) , 0) , (0 , 0) ]) ,chance = 0}

degreeToRadian::Float -> Float
degreeToRadian x = (pi*x)/180