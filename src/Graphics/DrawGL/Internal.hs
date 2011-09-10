{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Graphics.DrawGL.Internal where

import Control.Monad
import Control.Arrow
import Graphics.Rendering.OpenGL hiding (Color,Vertex)

import Graphics.DrawGL.Types

defaultColor = Color (1,1,1,1)
defaultCoord = TexCoord (0,0)

data Shape
    = Shape ShapeForm VertexData
    | Transformed (TexturedVertex -> TexturedVertex) Shape
    | ShapeSum Shape Shape
    | ShapeList [Shape]

data VertexData
    = Plain [Vertex]
    | Colored  [ColoredVertex]
    | Textured Texture [TexturedVertex]

class MakeShape m where
    mkShape :: ShapeForm -> m -> Shape

instance MakeShape [ColoredVertex] where
    mkShape f = Shape f . Colored

instance MakeShape [Vertex] where
    mkShape f = Shape f . Plain

instance MakeShape [Point] where
    mkShape f = Shape f . Plain . map Vertex

class ToTextured a where
    toTextured :: a -> TexturedVertex

instance ToTextured TexturedVertex where
    toTextured = id

instance ToTextured ColoredVertex where
    toTextured = (,) defaultCoord

instance ToTextured Vertex where
    toTextured = toTextured . (,) defaultColor

drawShape :: Shape -> IO ()
drawShape (ShapeSum a b)   = drawShape a >> drawShape b
drawShape (ShapeList ss)   = mapM_ drawShape ss
drawShape (Transformed t s) = drawShapeT t s
drawShape s                 = drawShapeT id s

drawShapeT :: (TexturedVertex -> TexturedVertex) -> Shape -> IO ()
drawShapeT ct (ShapeSum a b)    = drawShapeT ct a >> drawShapeT ct b
drawShapeT ct (ShapeList ss)    = mapM_ (drawShapeT ct) ss
drawShapeT ct (Transformed t s) = drawShapeT (ct . t) s
drawShapeT ct (Shape f vs)      = shapeOps vs where
    shapeOps (Plain vs) = do
        vertexColor defaultColor
        disableTexturing
        drawOps vs

    shapeOps (Colored vs) = do
        disableTexturing
        drawOps vs

    shapeOps (Textured t vs) = do
        setTexture t
        drawOps vs

    drawOps :: ToTextured t => [t] -> IO ()
    drawOps = renderPrimitive f . mapM_ vertexOps . map (ct . toTextured)

    vertexOps (t,(c,v)) = case vs of
        (Plain _)      -> vertexPos v
        (Colored _)    -> vertexColor c >> vertexPos v
        (Textured _ _) -> vertexTex t >> vertexColor c >> vertexPos v

    vertexTex           = texCoord . uncurry TexCoord2 . fromTexCoord
    vertexColor         = color . color4 . fromColor
    vertexPos           = vertex . uncurry Vertex2 . fromVertex
    color4 (r,g,b,a)    = Color4 r g b a

    disableTexturing    = texture Texture2D $= Disabled
    setTexture t        = textureBinding Texture2D $= Just t
