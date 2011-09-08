
module Graphics.DrawGL.Color where

import Graphics.DrawGL.Types

color :: Color -> ColoredVertex -> ColoredVertex
color c (_,v) = (c,v)

gradient :: Color -> Color -> (Point -> Float) -> (ColoredVertex -> ColoredVertex)
gradient a b f (_,v@(Vertex p)) = (mix a b (f p), v)

mix :: Color -> Color -> Float -> Color
mix a b c = (c' `mult` a) `add` (c `mult` b) where
    c' = 1.0 - c

add :: Color -> Color -> Color
add (Color (r,g,b,a)) (Color (r',g',b',a')) = Color ((r+r'),(g+g'),(b+b'),(a+a'))

mult :: Float -> Color -> Color
mult v (Color (r,g,b,a)) = Color ((r*v),(g*v),(b*v),(a*v))
