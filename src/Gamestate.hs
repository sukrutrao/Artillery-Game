module Gamestate where

import Physics
import Tank
import Weapon

data Tile = Tile {
    tileposition :: Point,
    isObstacle :: Bool
} deriving (Show)

data GameState = GameState {
    tileMatrix :: [[Tile]],
    tankList :: [Tank],
    weapon :: [Weapon],
    chance :: Int
} deriving (Show)

widthOfTile :: Float
widthOfTile = 1

heightOfTile :: Float
heightOfTile = 1

getTileMatrix :: [[Tile]] 
getTileMatrix = [[Tile {tileposition = (Position (-1) 0),isObstacle = False } , Tile {tileposition = (Position 0 0),isObstacle = False } ] ,
                 [Tile {tileposition = (Position (-1) (-1)),isObstacle = True } , Tile {tileposition = (Position 0 (-1)),isObstacle = True } ]
                ]

getTankList :: [(Float , Float)] -> [Tank]
getTankList initialPosition = [(initializeTank x y) | (x,y) <- initialPosition] 

getWeaponList :: [(Float , Float)] -> [Weapon]
getWeaponList initialWeaponPosition = [(initializeWeapon x y) | (x,y) <- initialWeaponPosition] 


initializeGamestate::GameState
initializeGamestate = GameState {tileMatrix = (getTileMatrix), tankList = (getTankList [(0 , 0) , (0.5 , 0)]), weapon = (getWeaponList [(0 , 0) , (0.5 , 0)]) ,chance = 0}