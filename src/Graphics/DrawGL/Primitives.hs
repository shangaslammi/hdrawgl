
module Graphics.DrawGL.Primitives where

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.DrawGL.Internal
import Graphics.DrawGL.Types
import Graphics.DrawGL.Util

mkLine (p1,p2) = [Vertex p1, Vertex p2]

pointsToShape :: ShapeForm -> Points -> Shape
pointsToShape f = Shape f . VertexList . map Vertex

line :: Point -> Point -> Shape
line p = Shape GL.Lines . VertexList . curry mkLine p

lines :: [(Point,Point)] -> Shape
lines = Shape GL.Lines . VertexList . concatMap mkLine

multiline :: Points -> Shape
multiline = pointsToShape GL.LineStrip

rectFill :: Point -> (Width, Height) -> Shape
rectFill pt sz = pointsToShape GL.Quads $ rectPoints pt sz

rect :: Point -> (Width, Height) -> Shape
rect pt sz = pointsToShape GL.LineLoop $ rectPoints pt sz

rectPoints :: Point -> (Width, Height) -> Points
rectPoints (x,y) (w,h) = [(x,y),(x+w,y),(x+w,y+h),(x,y+h)]

regularPoly :: Sides -> Radius -> Shape
regularPoly s r = multiline $ regularPolyPoints s r

regularPolyFill :: Sides -> Radius -> Shape
regularPolyFill s r = pointsToShape GL.TriangleFan . ((0,0):) $ pts where
    pts = regularPolyPoints s r

regularPolyPoints :: Sides -> Radius -> Points
regularPolyPoints s r = map fromPolar $ pts where
    pts  = take s $ zip (iterate (+step) 0) (repeat r)
    step = pi * 2 / fromIntegral s
