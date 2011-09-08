
module Graphics.DrawGL.Util where

import Graphics.DrawGL.Types

fromPolar :: (Angle, Radius) -> Point
fromPolar (a, r) = (-sin a * r, cos a * r)

between :: Ord a => (a,a) -> a -> a
between (min, max) v
    | v < min   = min
    | v > max   = max
    | otherwise = v
