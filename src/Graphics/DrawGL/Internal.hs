{-# LANGUAGE FlexibleInstances #-}
module Graphics.DrawGL.Internal where

import Control.Monad
import Control.Arrow
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.DrawGL.Types

defaultColor = Color (1,1,1,1)
defaultCoord = TexCoord (0,0)

data Shape
     = Shape ShapeForm [TexturedVertex]
     | Transformed (TexturedVertex -> TexturedVertex) Shape
     | ShapeSum Shape Shape
     | ShapeList [Shape]

class MakeShape m where
    mkShape :: ShapeForm -> m -> Shape

instance MakeShape [TexturedVertex] where
    mkShape = Shape

instance MakeShape [ColoredVertex] where
    mkShape f = Shape f . map ((,) defaultCoord)

instance MakeShape [Vertex] where
    mkShape f = mkShape f . map ((,) defaultColor)

instance MakeShape [Point] where
    mkShape f = mkShape f . map Vertex


drawShape :: Shape -> IO ()
drawShape (ShapeSum a b)   = drawShape a >> drawShape b
drawShape (ShapeList ss)   = mapM_ drawShape ss
drawShape (Transformed t s) = drawShapeT t s
drawShape s                 = drawShapeT id s

drawShapeT :: (TexturedVertex -> TexturedVertex) -> Shape -> IO ()
drawShapeT ct (ShapeSum a b)    = drawShapeT ct a >> drawShapeT ct b
drawShapeT ct (ShapeList ss)    = mapM_ (drawShapeT ct) ss
drawShapeT ct (Transformed t s) = drawShapeT (ct . t) s
drawShapeT ct (Shape f vs)      = GL.renderPrimitive f $ drawOps vs where
    drawOps             = mapM_ vertexOps . map ct
    vertexOps (t,(c,v)) = vertexColor c >> vertexPos v
    vertexColor         = GL.color . color4 . fromColor
    vertexPos           = GL.vertex . uncurry GL.Vertex2 . fromVertex
    color4 (r,g,b,a)    = GL.Color4 r g b a
