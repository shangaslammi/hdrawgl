
module Graphics.DrawGL.Color where

import Graphics.DrawGL.Types

color :: Color -> ColoredVertex -> ColoredVertex
color c' (c,v) = (c',v)
