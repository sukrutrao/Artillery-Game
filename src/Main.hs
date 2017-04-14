import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.IO.Unsafe
import Types
import Render
import Gamestate

main :: IO ()
main = play window background fps initializeGamestate render handlekeys update

window :: Display
window = InWindow "Tanki" ( 1000 , 500 ) (500, 500)

background :: Color
background = white

disp :: GameState -> Picture
disp game = rectangleSolid 10 50

fps :: Int
fps = 60




