module Line where
 
import Graphics.UI.GLUT

-- | Function to map coordinates to GLUT Vertex3 for positioning
vertex3f x y z = vertex $ Vertex3 x y (z :: Float)

-- | Function to draw a line of inputted length starting at the origin and in +ve X direction
line :: Float -> IO ()
line length = renderPrimitive Lines $ do
                            vertex3f 0 0 0
                            vertex3f length 0 0