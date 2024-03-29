
module Graphics.DrawGL.Types where

import qualified Graphics.Rendering.OpenGL as GL

type VertexType = Float

type Point  = (VertexType, VertexType)
type Points = [Point]
type Width  = VertexType
type Height = VertexType
type Radius = VertexType
type Sides  = Int
type Angle  = Float

type ColoredVertex = (Color, Vertex)
type ShapeForm = GL.PrimitiveMode
type VertexTransform = ColoredVertex -> ColoredVertex

newtype Vertex = Vertex { fromVertex :: (VertexType, VertexType) }
newtype Color = Color { fromColor :: (Float,Float,Float,Float) }

