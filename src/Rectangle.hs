module Rectangle where
 
import Graphics.UI.GLUT

-- | Function to map coordinates to GLUT Vertex3 for positioning
vertex3f x y z = vertex $ Vertex3 x y (z :: Float)

-- | Function to draw a rectangle of inputted dimensions starting at the origin and in 1st quadrant
rectangle :: Float -> Float -> IO ()
rectangle width height = renderPrimitive Quads $ do
                            vertex3f 0 0 0
                            vertex3f 0 height 0
                            vertex3f width height 0
                            vertex3f width 0 0