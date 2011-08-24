{-#LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances#-}

module Graphics.DrawGL.Concat where

import Graphics.DrawGL.Internal

import Data.Function (on)

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
