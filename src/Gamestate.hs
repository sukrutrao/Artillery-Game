data Turret = Turret { angle :: IORef GLfloat
                     , power :: Double  
                     }   

data Position = Position { x :: IORef GLfloat
                         , y :: IORef GLfloat  
                         }   

data Acceleration = Acceleration { a_x :: Double
                                 , a_y :: Double  
                                 } 

data Velocity = Velocity { v_x :: Double
                         , v_y :: Double  
                         } 

data State = Left | Right  
           deriving (Enum)

data TankState = { state :: State
                 , position :: Position
                 , velocity :: Velocity
                 , turret :: Turret
                 }

data Tank = { tankState :: TankState
            , radius :: Double
            , score :: Integer
            }

data Tile = { x :: IORef GLfloat
            , y :: IORef GLfloat
            , z :: IORef GLfloat
            , isObstacle :: Bool  
            }

data Bullet = { }


