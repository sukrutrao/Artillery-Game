module Rectangle (rectangle) where
 
import Graphics.UI.GLUT

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
 
rectangle :: GLfloat -> GLfloat -> GLfloat -> IO ()

rectangle bottomLeft width height = renderPrimitive Quads $ mapM_ vertex3f
  [ ( bottomLeft, bottomLeft, 0), ( bottomLeft, bottomLeft+height, 0), ( bottomLeft+width, bottomLeft+height, 0), ( bottomLeft+width, bottomLeft , 0)]