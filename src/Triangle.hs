module Triangle where
 
import Graphics.UI.GLUT

-- | Function to map coordinates to GLUT Vertex3 for positioning
vertex3f :: (Float, Float, Float) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

-- | Function to find coordinates of equilaterla triangle with tip at the origin
coord::Float->[(Float,Float,Float)]
coord edge = [(0,0,0) , (edge * (cos(pi/3)),edge * (sin(pi/3)),0) , ((-1)*edge * (cos(pi/3)),edge * (sin(pi/3)),0)]

-- | Function to draw a triangle of inputted edge length starting at the origin and in 1st quadrant
triangle :: Float -> IO ()
triangle edge = renderPrimitive Triangles $ mapM_ vertex3f (coord edge)