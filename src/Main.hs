{-# LANGUAGE TemplateHaskell #-}

import OrcaEnergy.Parsing
import OrcaEnergy.Plotting
import OrcaEnergy

import Diagrams.Prelude
import Diagrams.Backend.SVG

import HFlags

defineFlag "fname" "orcalog.out"   "Orca output file"
defineFlag "w:width" (500 :: Int)  "Width of generated diagram"
defineFlag "f:first" ((-4) :: Int) "Energy levels before HOMO to plot"
defineFlag "t:total" (8 :: Int)    "Number of orbitals to plot"

--fname = "peroxide_peroxide_orca.out"
fname = "superoxide_superoxide_orca.out"
main :: IO ()
main = do
  s <- $initHFlags "Orca Energy Plotter v0.1"
  orbs <- loadEnergies fname
  putStrLn $ show $ takeOrbs 2 4 orbs
  renderSVG "output.svg" (Height 400) $ plotOrbs (takeOrbs 4 10 orbs)

testPlot :: IO ()
testPlot = do
  putStrLn "Hello world!"
  renderSVG "energy.svg" (Width 400) $ alphaBeta --alpha
  renderSVG "energies.svg" (Height 500) $ energyDiagram

  putStrLn "Done."


energyDiagram = foldl go mempty energies
  where
    go acc e = alphaBeta # translate (r2 (0,e)) `atop` acc

energies = [0.0, 0.75 .. 1.7]

