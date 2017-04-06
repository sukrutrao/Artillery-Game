module Health where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Rectangle

hcalc :: GLfloat -> GLfloat -> GLfloat
hcalc x y = x * y/100

color3f :: GLfloat -> GLfloat -> GLfloat -> IO()
color3f r g b = color $ Color3 r g b
vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO()
vertex3f x y z = vertex $ Vertex3 x y z

healthbar :: GLfloat -> GLfloat -> GLfloat -> IO()
healthbar x y h = renderPrimitive Quads $ do
  
    rectangle x y 0.1 0.025 1 1 1
    rectangle x y (hcalc 0.1 h) 0.025 0 1 0
