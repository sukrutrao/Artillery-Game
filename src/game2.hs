import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.IO.Unsafe

main :: IO ()
main = play window background fps initial render handlekeys update

window :: Display
window = InWindow "Pong" (500, 500) (500, 500)

background :: Color
background = white

fps :: Int
fps = 1

data Ring = Make
  { size :: (Float, Float)
    ,position :: (Float, Float)
    ,angle :: Float
    ,filepath :: String
  }

initial :: Ring
initial = Make
  { size = (1,1)
    ,position = (250,250)
    ,angle = 0
    ,filepath = "pen.bmp"
  }

render :: Ring -> Picture
render world = pictures [ ( scale w h $ translate (x) (y) $ rotate a $ (unsafePerformIO (loadBMP "pen.bmp")) ) ,rectangleSolid 50 50 ]
  where
    (x,y) = position world
    (w,h) = size world
    a = angle world

handlekeys :: Event -> Ring -> Ring
handlekeys (EventKey (Char 'd') Down _ _) game = game { position = (x',y) }
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
    h' = h+0.1

handlekeys (EventKey (Char 'k') _ _ _) game = game { size = (w,h') }
  where
    (w,h) = size game
    h' = h-0.1

handlekeys (EventKey (Char 'j') _ _ _) game = game { size = (w',h) }
  where
    (w,h) = size game
    w' = w - 0.1

handlekeys (EventKey (Char 'l') _ _ _) game = game { size = (w',h) }
  where
    (w,h) = size game
    w' = w + 0.1

handlekeys (EventKey (Char 'e') _ _ _) game = game { angle = a' }
  where
    a = angle game
    a' = a + 90

handlekeys (EventKey (Char 'q') _ _ _) game = game { angle = a' }
  where
    a = angle game
    a' = a - 90

handlekeys _ x = x

update :: Float -> Ring -> Ring
update _ game = game
