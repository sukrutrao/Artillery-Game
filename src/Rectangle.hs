module Rectangle (rectangle) where
 
import Graphics.UI.GLUT

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
color3f :: GLfloat -> GLfloat -> GLfloat -> Color3 GLfloat
color3f r g b = Color3 r g b
 
rectangle :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

rectangle bottomLeftX bottomLeftY width height r g b = do
  currentColor $= Color4 r g b 1
  renderPrimitive Quads $ mapM_ vertex3f [ ( bottomLeftX, bottomLeftY, 0), ( bottomLeftX, bottomLeftY+height, 0), ( bottomLeftX+width, bottomLeftY+height, 0), ( bottomLeftX+width, bottomLeftY , 0)]
