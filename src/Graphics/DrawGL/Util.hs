
module Graphics.DrawGL.Util where

import Graphics.DrawGL.Types

fromPolar :: (Angle, Radius) -> Point
fromPolar (a, r) = (-sin a * r, cos a * r)