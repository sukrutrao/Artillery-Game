import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

main :: IO ()
main = play window background fps initial render handlekeys update

window :: Display
window = InWindow "Pong" (500, 500) (500, 500)

background :: Color
background = white

fps :: Int
fps = 60

data Ring = Make
  { size :: (Float, Float)
    ,position :: (Float, Float)
    ,angle :: Float
  }

initial :: Ring
initial = Make
  { size = 50
    ,position = (0,0)
    ,angle = 0
  }

render :: Ring -> Picture
render world = translate (x) (y) $ rotate a $ rectangleSolid w h 
  where
    (x,y) = position world
    (w,h) = size world
    a = angle world

handlekeys :: Event -> Ring -> Ring
handlekeys (EventKey (Char 'd') _ _ _) game = game { position = (x',y) }
  where
    (x,y) = position game
    x' = x+10

handlekeys (EventKey (Char 'a') _ _ _) game = game { position = (x',y) }
  where
    (x,y) = position game
    x' = x-10

handlekeys (EventKey (Char 'w') _ _ _) game = game { position = (x,y') }
  where
    (x,y) = position game
    y' = y+10

handlekeys (EventKey (Char 's') _ _ _) game = game { position = (x,y') }
  where
    (x,y) = position game
    y' = y-10

handlekeys (EventKey (Char 'i') _ _ _) game = game { size = (w,h') }
  where
    (w,h) = size game
    h' = h+10

handlekeys (EventKey (Char 'k') _ _ _) game = game { size = (w,h') }
  where
    (w,h) = size game
    h' = h-10

handlekeys (EventKey (Char 'j') _ _ _) game = game { size = (w',h) }
  where
    (w,h) = size game
    w' = w-10

handlekeys (EventKey (Char 'l') _ _ _) game = game { size = (w',h) }
  where
    (w,h) = size game
    w' = w+10

handlekeys (EventKey (Char 'e') _ _ _) game = game { angle = a' }
  where
    a = angle game
    a' = a + 10

handlekeys (EventKey (Char 'q') _ _ _) game = game { angle = a' }
  where
    a = angle game
    a' = a - 10

handlekeys _ x = x

update :: Float -> Ring -> Ring
update _ game = game
