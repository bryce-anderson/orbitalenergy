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

plotOrb :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => Orbital -> QDiagram b V2 Double Any
plotOrb o = p (occupancy o, spin o)
  where
    p (Occupied, Nothing)    = addEnergy alphaBeta
    p (EmptyOrb, Nothing)    = addEnergy emptyLevel
    p (Occupied, Just Alpha) = addEnergy alpha # translate (r2 (-alphaBetaSpacing, 0.0))
    p (EmptyOrb, Just Alpha) = addEnergy emptyLevel # translate (r2 (-alphaBetaSpacing, 0.0))
    p (Occupied, Just Beta)  = addEnergy beta  # translate (r2 (alphaBetaSpacing, 0.0))
    p (EmptyOrb, Just Beta)  = addEnergy emptyLevel # translate (r2 (alphaBetaSpacing, 0.0))

    alphaBetaSpacing = 0.6

    addEnergy :: Renderable (Text Double) b => QDiagram b V2 Double Any -> QDiagram b V2 Double Any
    addEnergy d = d <> t
      where
        xshiftpos = 0.9
        yshiftpos = (-0.05)
        e = energy o
        t' = (text (printf "%0.2f eV" e) # fontSize (local 0.15)) `atop` strutX xshiftpos
        t = case spin o of
              Just Alpha -> t' # translate (r2 (-xshiftpos, yshiftpos))
              _          -> t' # translate (r2 (xshiftpos, yshiftpos))

-- plotOrbs :: (Backend b V2, Renderable (Path (V2 Double)) b, Renderable Text b) => [Orbital] -> Diagram b (V2 Double)
plotOrbs :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => [Orbital] -> QDiagram b V2 Double Any
plotOrbs orbs = foldl1 atop (mapper <$> orbs) `atop` (plotAxis orbs # translate (r2 (-1.0,0)))
  where
    mapper orb = plotOrb orb # translate (r2 (0.0, position (energy orb)))
    position eng = (eng - bottom) * scaleFactor
    scaleFactor = height / (top - bottom)
    (bottom,top) = energyBounds orbs
    height = 1

plotAxis :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => [Orbital] -> QDiagram b V2 Double Any
plotAxis orbs = (strokeLine engLine) # lw thin
  where
    offset = 0
    engLine = lineFromVertices [ p2 (0,0), p2 (0,1.5)]


energyBounds :: [Orbital] -> (Double,Double)
energyBounds orbs = (minimum es, maximum es)
  where es = map energy orbs

