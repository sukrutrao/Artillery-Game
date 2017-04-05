module Gamestate   
( Turret(..)
, Position(..)  
, Acceleration(..)  
, Velocity(..)    
, State(..)    
, TankState(..)   
, Tank(..)  
, Tile(..)  
) where  

data Turret = Turret { angle :: Double
                     , power :: Double  
                     }   

data Position = Position { x :: Double
                         , y :: Double
                         }   

data Acceleration = Acceleration { a_x :: Double
                                 , a_y :: Double  
                                 } 

data Velocity = Velocity { v_x :: Double
                         , v_y :: Double  
                         } 

data State = Left | Right  
            deriving (Enum)

data TankState = TankState { state :: State
                           , position :: Position
                           , velocity :: Velocity
                           , turret :: Turret
                           }

data Tank  = Tank { tankState :: TankState
                  , radius :: Double
                  , score :: Integer
                  }

data Tile = Tile { tile_x :: Double
                 , tile_y :: Double
                 , tile_z :: Double
                 , isObstacle :: Bool  
                 }
