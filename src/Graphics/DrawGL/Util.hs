
module Graphics.DrawGL.Util where

import Data.Maybe

import Graphics.DrawGL.Fold
import Graphics.DrawGL.Internal
import Graphics.DrawGL.Types

fromPolar :: (Angle, Radius) -> Point
fromPolar (a, r) = (-sin a * r, cos a * r)

between :: Ord a => (a,a) -> a -> a
between (min, max) v
    | v < min   = min
    | v > max   = max
    | otherwise = v

boundingBox :: Shape -> (Point, Point)
boundingBox = fromJust . foldShape step Nothing where
    step Nothing (Vertex p) = Just (p,p)
    step (Just ((minx,miny),(maxx,maxy))) (Vertex (x,y)) =
        Just ((min x minx, min y miny), ((max x maxx, max y maxy)))
