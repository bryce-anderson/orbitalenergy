
import OrcaEnergy.Parsing
import OrcaEnergy.Plotting

import Diagrams.Prelude
import Diagrams.Backend.SVG


fname = "peroxide_peroxide_orca.out"
--fname = "superoxide_superoxide_orca.out"
main :: IO ()
main = do
  putStrLn "Hello"
  orbs <- loadEnergies fname
  putStrLn $ show $ takeOrbs 2 4 orbs

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

