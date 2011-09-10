{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.DrawGL.Fold where

import Control.Arrow
import Data.List

import Graphics.DrawGL.Internal
import Graphics.DrawGL.Types

class ShapeFold a where
    normalizeFold :: (b -> a -> b) -> (b -> TexturedVertex -> b)

    foldVertices :: (b -> a -> b) -> b -> VertexData -> b

instance ShapeFold Vertex where
    normalizeFold f a = f a . snd . snd

    foldVertices f a v = case v of
        (Plain vs)      -> foldl' f a vs
        (Colored vs)    -> foldl' f' a vs where f' a = f a . snd
        (Textured _ vs) -> foldl' f' a vs where f' a = f a . snd . snd

instance ShapeFold ColoredVertex where
    normalizeFold f a = f a . snd

    foldVertices f a v = case v of
        (Plain vs)      -> foldl' f' a vs where f' a = f a . (,) defaultColor
        (Colored vs)    -> foldl' f a vs
        (Textured _ vs) -> foldl' f' a vs where f' a = f a . snd

instance ShapeFold TexturedVertex where
    normalizeFold = id

    foldVertices f a v = case v of
        (Plain vs)      -> foldl' f' a vs where f' a = f a . toTextured
        (Colored vs)    -> foldl' f' a vs where f' a = f a . toTextured
        (Textured _ vs) -> foldl' f a vs

foldShape :: ShapeFold a => (b -> a -> b) -> b -> Shape -> b
foldShape f a s = case s of
    (Shape _ vs)      -> foldVertices f a vs
    (Transformed g s) -> foldShape g' a s where g' a = f' a . g
    (ShapeList ss)    -> foldl' (foldShape f) a ss
    (ShapeSum s1 s2)  -> foldShape f a' s2 where a' = foldShape f a s1
    where f' = normalizeFold f
