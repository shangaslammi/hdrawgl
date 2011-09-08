
module Graphics.DrawGL.Fold where

import Data.List

import Graphics.DrawGL.Internal
import Graphics.DrawGL.Types

class ShapeFold a where
    foldShape :: (b -> a -> b) -> b -> Shape -> b


instance ShapeFold Vertex where
    foldShape f a (Shape _ vs)
        = foldVertices f a vs
    foldShape f a (ShapeSum s1 s2)
        = let a' = foldShape f a s1 in foldShape f a' s2
    foldShape f a (ShapeList ss)
        = foldl' (foldShape f) a ss

foldVertices :: (b -> Vertex -> b) -> b -> Vertices -> b
foldVertices f a v = case v of
    VertexList vs   -> foldl' f a vs
    VertexSum v1 v2 -> let a' = foldVertices f a v1 in foldVertices f a' v2
