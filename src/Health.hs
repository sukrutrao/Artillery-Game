module Health where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

hcalc :: GLfloat -> GLfloat -> GLfloat
hcalc x y = x * y/100

color3f :: GLfloat -> GLfloat -> GLfloat -> IO()
color3f r g b = color $ Color3 r g b
vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO()
vertex3f x y z = vertex $ Vertex3 x y z

healthbar :: GLfloat -> GLfloat -> GLfloat -> IO()
healthbar x y h = renderPrimitive Quads $ do
    color3f 1 1 1
    vertex3f x (y-0.025) 0
    vertex3f x y 0
    vertex3f (x+0.1) y 0
    vertex3f (x+0.1) (y-0.025) 0
    
    color3f 0 1 0
    vertex3f x (y-0.025) 0
    vertex3f x y 0
    vertex3f (x+(hcalc 0.1 h)) y 0
    vertex3f (x+(hcalc 0.1 h)) (y-0.025) 0
