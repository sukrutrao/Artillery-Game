module Display (createTileMap, createTank , createPowerBar , createScore , createBullet) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Gamestate
import Rectangle
import Point
 
createTileMap tileMatrix = do
                    putStrLn "TileMap Graphics Coming Soon"

createTank tileMatrix = do
                    putStrLn "TileMap Graphics Coming Soon"

createPowerBar powerMatrix = do
                    putStrLn "PowerBar Graphics Coming Soon"

createScore scoreMatrix = do
                    putStrLn "Score Graphics Coming Soon"

createBullet :: (GLfloat,GLfloat) -> GLfloat -> IO ()
createBullet phyBulletFunc rotationAngle = 
                                let x' =  fst phyBulletFunc; y' =  snd phyBulletFunc  
                                in
                                    do
                                loadIdentity                            
                                scale 2 2 (1::GLfloat)
                                translate $ Vector3 x' y' (0::GLfloat)
                                rotate rotationAngle $ Vector3 0 0 (1::GLfloat)
                                currentColor $= Color4 0.5 0.5 0.1 1
                                point x' y'
                                flush
