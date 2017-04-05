module Point (point) where
 
import Graphics.UI.GLUT

point :: GLfloat -> GLfloat -> IO ()
point xCoordinate yCoordinate = do
                            pointSize $= 20
                            renderPrimitive Points (vertex (Vertex3 xCoordinate yCoordinate (0::GLfloat)))