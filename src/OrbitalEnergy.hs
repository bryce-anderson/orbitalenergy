{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OrbitalEnergy (
    Spin(..),
    OrbOcc(..),
    Orbital(..),
    plotOrbs
) where

import Text.Printf

import Diagrams.Core.Types (Renderable, Diagram)
import Diagrams.TwoD.Text
import Diagrams.TwoD.Types (V2)
import Diagrams.Path (Path)
import Diagrams.Prelude

import OrbitalEnergy.Plotting
import qualified Debug.Trace as T

data Spin = Alpha
          | Beta
     deriving (Eq, Show)

data OrbOcc = Occupied
            | EmptyOrb
     deriving (Eq, Show)

data Orbital = Orbital { energy    :: Double
                       , occupancy :: OrbOcc
                       , spin      :: Maybe Spin
                       } deriving (Show, Eq)

instance Ord Orbital where
  compare a b = energy a `compare` energy b

plotOrb :: Renderable (Path V2 Double) b => Orbital -> QDiagram b V2 Double Any
plotOrb o = pe (occupancy o, spin o)
  where
    pe = p

    p (Occupied, Nothing)    = alphaBeta
    p (EmptyOrb, Nothing)    = emptyLevel
    p (Occupied, Just Alpha) = alpha # translate (r2 (-alphaBetaSpacing, 0.0))
    p (EmptyOrb, Just Alpha) = emptyLevel # translate (r2 (-alphaBetaSpacing, 0.0))
    p (Occupied, Just Beta)  = beta  # translate (r2 (alphaBetaSpacing, 0.0))
    p (EmptyOrb, Just Beta)  = emptyLevel # translate (r2 (alphaBetaSpacing, 0.0))

    alphaBetaSpacing = 0.6

plotOrbs :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => [Orbital] -> QDiagram b V2 Double Any
plotOrbs orbs = foldl1 atop (mapper <$> orbs)
  where
    mapper orb = plotOrb orb # translate (r2 (0.0, position (energy orb)))
    position eng = (eng - bottom) * scaleFactor
    scaleFactor = height / (top - bottom)
    (bottom,top) = energyBounds orbs
    height = 1

energyBounds :: [Orbital] -> (Double,Double)
energyBounds orbs = (minimum es, maximum es)
  where es = map energy orbs

