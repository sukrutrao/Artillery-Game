import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Health
import Power
import Rectangle

hcalc :: GLfloat -> GLfloat -> GLfloat
hcalc x y = x * y/100

x :: GLfloat
x = 0.5
y :: GLfloat
y = 0.5

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  reshapeCallback $= Just reshape
  mainLoop
 
reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

display :: IO ()
display = do
  clear [ColorBuffer , DepthBuffer]
  rectangle 0 0 0.5 0.5 0 0 1
  healthbar 0.5 0.5 70
  powerbar 60

  -- Clear the screen with the default clear color (black)
  --x1 :: GLfloat 
  {-let color3f r g b = color $ Color3 r g (b :: GLfloat)
  let vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
  clear [ColorBuffer , DepthBuffer]
  renderPrimitive Quads $ do
    color3f 1 1 1
    vertex3f x (y-0.025) 0
    vertex3f x y 0
    vertex3f (x+0.1) y 0
    vertex3f (x+0.1) (y-0.025) 0
    
    color3f 0 1 0
    vertex3f x (y-0.025) 0
    vertex3f x y 0
    vertex3f (x+(hcalc 0.1 60)) y 0
    vertex3f (x+(hcalc 0.1 60)) (y-0.025) 0-}

  flush


