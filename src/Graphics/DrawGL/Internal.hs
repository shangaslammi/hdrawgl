module Graphics.DrawGL.Internal where

import Control.Monad
import Control.Arrow
import qualified Graphics.Rendering.OpenGL as GL

data Shape
     = Shape ShapeForm Vertices
     | ShapeSum Shape Shape
     | ShapeList [Shape]
     | Transformed VertexTransform Shape

data Vertices
    = VertexList [Vertex]
    | VertexSum Vertices Vertices


type ColoredVertex = (Color, Vertex)
type ShapeForm = GL.PrimitiveMode
type VertexTransform = ColoredVertex -> ColoredVertex

type VertexType = Float
newtype Vertex = Vertex { fromVertex :: (VertexType, VertexType) }
newtype Color = Color { fromColor :: (Float,Float,Float,Float) }


drawShape :: Shape -> IO ()
drawShape (ShapeSum a b)   = drawShape a >> drawShape b
drawShape (ShapeList ss)   = mapM_ drawShape ss
drawShape (Transformed t s) = drawShapeT t s
drawShape s                 = drawShapeT id s

drawShapeT :: VertexTransform -> Shape -> IO ()
drawShapeT ct (ShapeSum a b)   = drawShapeT ct a >> drawShapeT ct b
drawShapeT ct (ShapeList ss)   = mapM_ (drawShapeT ct) ss
drawShapeT ct (Transformed t s) = drawShapeT (ct . t) s
drawShapeT ct (Shape f vs)      = GL.renderPrimitive f $ drawOps vs where
    drawOps (VertexList l)  = mapM_ vertexOps . map ct . defaultColor $ l
    drawOps (VertexSum a b) = drawOps a >> drawOps b
    defaultColor = zip $ map Color $ repeat (1,1,1,1)
    vertexOps (c,v) = vertexColor c >> vertexPos v
    vertexColor = GL.color . color4 . fromColor
    vertexPos = GL.vertex . uncurry GL.Vertex2 . fromVertex
    color4 (r,g,b,a) = GL.Color4 r g b a
