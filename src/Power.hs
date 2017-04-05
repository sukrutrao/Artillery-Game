module Power where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

hcalc :: GLfloat -> GLfloat -> GLfloat
hcalc x y = x * y/100

color3f :: GLfloat -> GLfloat -> GLfloat -> IO()
color3f r g b = color $ Color3 r g b
vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO()
vertex3f x y z = vertex $ Vertex3 x y z

powerbar :: GLfloat -> IO()
powerbar p = renderPrimitive Quads $ do
    color3f 1 1 1
    vertex3f 0.85 0 0
    vertex3f 0.85 0.5 0
    vertex3f 0.95 0.5 0
    vertex3f 0.95 0 0
 
    color3f 1 0 0
    vertex3f 0.85 0 0
    vertex3f 0.85 (hcalc 0.5 p) 0
    vertex3f 0.95 (hcalc 0.5 p) 0
    vertex3f 0.95 0 0
