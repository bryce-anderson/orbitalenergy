{-# LANGUAGE FlexibleContexts #-}

module OrcaEnergy (
    Spin(..),
    OrbOcc(..),
    Orbital(..)
) where

import Diagrams.Core.Types (Renderable, Diagram)
import Diagrams.TwoD.Types (R2)
import Diagrams.Path (Path)


data Spin = Alpha
          | Beta
     deriving (Eq, Show)

data OrbOcc = Occupied
            | Empty
     deriving (Eq, Show)

data Orbital = Orbital { energy    :: Double
                       , occupancy :: OrbOcc 
                       , spin      :: Maybe Spin
                       } deriving (Show, Eq)

instance Ord Orbital where
  compare a b = energy a `compare` energy b

-- alphaBeta :: Renderable (Path R2) b => Diagram b R2
plotOrbs :: Renderable (Path R2) b => [Orbital] -> Diagram b R2
plotOrbs = undefined


