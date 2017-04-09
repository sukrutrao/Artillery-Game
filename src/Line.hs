module Line where
 
import Graphics.UI.GLUT

vertex3f x y z = vertex $ Vertex3 x y (z :: Float)

line :: Float -> IO ()
line length = renderPrimitive Lines $ do
                            vertex3f 0 0 0
                            vertex3f length 0 0