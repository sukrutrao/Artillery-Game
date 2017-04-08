module Rectangle where
 
import Graphics.UI.GLUT

vertex3f x y z = vertex $ Vertex3 x y (z :: Float)

rectangle :: Float -> Float -> IO ()
rectangle width height = renderPrimitive Quads $ do
                            vertex3f 0 0 0
                            vertex3f 0 height 0
                            vertex3f width height 0
                            vertex3f width 0 0