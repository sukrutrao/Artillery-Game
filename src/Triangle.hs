module Triangle where
 
import Graphics.UI.GLUT

vertex3f :: (Float, Float, Float) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

coord::Float->[(Float,Float,Float)]
coord edge = [(0,0,0) , (edge * (cos(pi/3)),edge * (sin(pi/3)),0) , ((-1)*edge * (cos(pi/3)),edge * (sin(pi/3)),0)]

triangle :: Float -> IO ()
triangle edge = renderPrimitive Triangles $ mapM_ vertex3f (coord edge)