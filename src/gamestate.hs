data Turret = Turret { angle :: IORef GLfloat  
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

data Direction = FacingLeft | FacingRight  
           deriving (Enum)

data TankState = TankState { direction :: Direction
                 , position :: Position
                 , velocity :: Velocity
                 , turret :: Turret
                 }

data Tank = Tank { tankState :: TankState
            , radius :: Double
            , score :: Integer
            }

powerIncrement :: Double
powerIncrement = 1

angleIncrement :: Double
angleIncrement = 1


