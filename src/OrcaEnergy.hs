{-# LANGUAGE FlexibleContexts #-}

module OrcaEnergy (
    Spin(..),
    OrbOcc(..),
    Orbital(..),
    plotOrbs
) where

import Text.Printf

import Diagrams.Core.Types (Renderable, Diagram)
import Diagrams.TwoD.Text 
import Diagrams.TwoD.Types (R2)
import Diagrams.Path (Path)
import Diagrams.Prelude

import OrcaEnergy.Plotting
import qualified Debug.Trace

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

alphaBetaSpacing = 0.6
vSpacingFactor = 25.0

plotOrb :: (Backend b R2, Renderable (Path R2) b, Renderable Text b) => Orbital -> Diagram b R2
plotOrb o = p (occupancy o, spin o)
  where
    p (Occupied, Nothing)    = addEnergy alphaBeta
    p (Empty, Nothing)       = addEnergy emptyLevel
    p (Occupied, Just Alpha) = addEnergy alpha # translate (r2 (-alphaBetaSpacing, 0.0))
    p (Empty, Just Alpha)    = addEnergy emptyLevel # translate (r2 (-alphaBetaSpacing, 0.0))
    p (Occupied, Just Beta)  = addEnergy beta  # translate (r2 (alphaBetaSpacing, 0.0)) 
    p (Empty, Just Beta)     = addEnergy emptyLevel # translate (r2 (alphaBetaSpacing, 0.0)) 

    addEnergy :: (Backend b R2, Renderable (Path R2) b, Renderable Text b) => Diagram b R2 -> Diagram b R2
    addEnergy d = d <> t
      where
        xshiftpos = 0.9
        yshiftpos = (-0.05)
        e = energy o
        t' = (text (printf "%0.2f eV" e) # fontSize (Local 0.15)) `atop` strutX xshiftpos
        t = case spin o of
              Just Alpha -> t' # translate (r2 (-xshiftpos, yshiftpos)) 
              _          -> t' # translate (r2 (xshiftpos, yshiftpos))

plotOrbs :: (Backend b R2, Renderable (Path R2) b, Renderable Text b) => [Orbital] -> Diagram b R2
plotOrbs os = foldl1 atop ds
  where
    c = length os
    (b,t) = erange os
    f e = b + e * vSpacingFactor / ((fromIntegral c) * (t-b)) 
    mapper o = plotOrb o # translate (r2 (0.0, f (energy o)))
    ds = mapper `map` os 

erange os = (minimum es, maximum es)
  where es = map energy os
