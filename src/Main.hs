import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.IO.Unsafe
import Types
import Gamestate

main :: IO ()
main = play window background fps initializeGamestate disp handlekeys update

window :: Display
window = InWindow "Tanki" (200, 200) (500, 500)

background :: Color
background = white

disp :: GameState -> Picture
disp game = rect 10 50

fps :: Int
fps = 60

handlekeys :: Event -> Types.GameState -> Types.GameState
handlekeys _ game = game

update :: Float -> Types.GameState -> Types.GameState
update _ game = game




