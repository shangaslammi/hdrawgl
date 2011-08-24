{-#LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances#-}
module Graphics.DrawGL where

import Data.Function (on)
import Control.Monad
import Control.Arrow
import qualified Graphics.Rendering.OpenGL as GL

data Shape
     = Shape ShapeForm Vertices
     | Transformed Transformation Shape
     | Colored Coloration Shape
     | TwoShapes Shape Shape
     | ManyShapes [Shape]

data Vertices
    = VertexList [Vertex]
    | VertexSum Vertices Vertices

type ShapeForm = GL.PrimitiveMode
type Transformation = [Vertex] -> [Vertex]
type Coloration = [Vertex] -> [Color]
type CombinedTransform = ([Vertex], [Color]) -> ([Vertex],[Color])

type VertexType = Float
newtype Vertex = Vertex { fromVertex :: (VertexType, VertexType) }
newtype Color = Color { fromColor :: (Float,Float,Float,Float) }

class Concat a b c where
    (<++>) :: Concat a b c => a -> b -> c

instance Concat Vertices Vertices Vertices where
    (<++>) = VertexSum

instance Concat Vertices [Vertex] Vertices where
    a <++> b = VertexSum a $ VertexList b

instance Concat [Vertex] Vertices Vertices where
    a <++> b = VertexSum (VertexList a) b

instance Concat [Vertex] [Vertex] Vertices where
    (<++>) = VertexSum `on` VertexList

instance Concat Shape Shape Shape where
    (<++>) = TwoShapes

instance Concat Shape [Shape] Shape where
    a <++> b = TwoShapes a $ ManyShapes b

instance Concat [Shape] Shape Shape where
    a <++> b = TwoShapes (ManyShapes a) b

instance Concat [Shape] [Shape] Shape where
    (<++>) = TwoShapes `on` ManyShapes

replaceLeft :: ([a] -> [b]) -> ([c],[a]) -> ([b],[a])
replaceLeft f = snd >>> f &&& id

replaceRight :: ([a] -> [b]) -> ([a],[c]) -> ([a],[b])
replaceRight f = fst >>> id &&& f

drawShape :: Shape -> IO ()
drawShape (TwoShapes a b)   = drawShape a >> drawShape b
drawShape (ManyShapes ss)   = mapM_ drawShape ss
drawShape (Transformed t s) = drawShapeT (first t) s
drawShape (Colored c s)     = drawShapeT (replaceRight c) s
drawShape s                 = drawShapeT id s

drawShapeT :: CombinedTransform -> Shape -> IO ()
drawShapeT ct (TwoShapes a b)   = drawShapeT ct a >> drawShapeT ct b
drawShapeT ct (ManyShapes ss)   = mapM_ (drawShapeT ct) ss
drawShapeT ct (Transformed t s) = drawShapeT (ct . first t) s
drawShapeT ct (Colored c s)     = drawShapeT (ct . replaceRight c) s
drawShapeT ct (Shape f vs)      = GL.renderPrimitive f $ drawOps vs where
    drawOps (VertexList l)  = mapM_ vertexOps . uncurry zip . ct . defaultColor $ l
    drawOps (VertexSum a b) = drawOps a >> drawOps b
    defaultColor = flip (,) $ map Color $ repeat (1,1,1,1)
    vertexOps (v,c) = vertexColor c >> (GL.vertex . uncurry GL.Vertex2 . fromVertex $ v)
    vertexColor = GL.color . color4 . fromColor
    color4 (r,g,b,a) = GL.Color4 r g b a
