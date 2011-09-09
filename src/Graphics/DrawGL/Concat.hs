{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Graphics.DrawGL.Concat where

import Graphics.DrawGL.Internal
import Graphics.DrawGL.Types

import Data.Function (on)

class Concat a b c where
    (<++>) :: a -> b -> c

instance Concat Shape Shape Shape where
    (<++>) = ShapeSum

instance Concat Shape [Shape] Shape where
    a <++> b = ShapeSum a $ ShapeList b

instance Concat [Shape] Shape Shape where
    a <++> b = ShapeSum (ShapeList a) b

instance Concat [Shape] [Shape] Shape where
    (<++>) = ShapeSum `on` ShapeList
