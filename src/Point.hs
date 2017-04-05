module Point (point) where
 
import Graphics.UI.GLUT

point :: GLfloat -> GLfloat -> GLfloat -> IO ()
point xCoordinate yCoordinate size = do
                                pointSize $= size
                                renderPrimitive Points $ do vertex $ Vertex3 xCoordinate, yCoordinate, 0