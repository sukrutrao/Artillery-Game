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

data State = Left | Right  
           deriving (Enum)

data TankState = { state :: State
                 , position :: Position
                 , velocity :: Velocity
                 , turret :: Turret
                 }

data tank = { tankState :: TankState
            , radius :: Double
            , score :: Integer
            } 

