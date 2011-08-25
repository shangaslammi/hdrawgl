{-# LANGUAGE FlexibleInstances #-}
module Graphics.DrawGL.Transform where

import Control.Arrow

import Graphics.DrawGL.Internal

type Angle = Float

class Transform t where
    normalizeTransform :: t -> Shape -> Shape

instance Transform (Vertex -> Vertex) where
    normalizeTransform = transformVertices

instance Transform (ColoredVertex -> ColoredVertex) where
    normalizeTransform f = Transformed f

instance Transform (Shape -> Shape) where
    normalizeTransform = ($)

transformVertices :: (Vertex -> Vertex) -> (Shape -> Shape)
transformVertices f = Transformed (second f)

rotate :: Angle -> Vertex -> Vertex
rotate a (Vertex (x,y)) = Vertex (x', y') where
    x' = x * cos a' - y * sin a'
    y' = x * sin a' + y * cos a'
    a' = 2 * pi * a

