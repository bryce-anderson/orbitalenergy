{-# LANGUAGE TemplateHaskell #-}

import OrcaEnergy.Parsing
import OrcaEnergy.Plotting
import OrcaEnergy

import Diagrams.Prelude
import Diagrams.Backend.SVG

import System.Directory
import Data.String.Utils

import HFlags

defineFlag "h:height" (500 :: Double) "Height of generated diagram"
defineFlag "f:first"  (4 :: Int)      "Energy levels before HOMO to plot"
defineFlag "t:total"  (8 :: Int)      "Number of orbitals to plot"

main :: IO ()
main = do
  s <- $initHFlags "Orca Energy Plotter v0.1"
  fnames <- case s of
              [] -> findFiles
              fs -> return fs 
  putStrLn $ "Attempting to plot orbitals of files " ++ show fnames
  runPlots fnames
  putStrLn "Done."

findFiles :: IO [FilePath]
findFiles =  filter (endswith ".out") `fmap` getDirectoryContents "."

outFile = "enrgylevels.svg" 

runPlots :: [FilePath] -> IO ()
runPlots fnames = do
  orbs <- loadOrbs `mapM` fnames
  let maxWidth = maximum $ width `map` orbs
      total = foldl (\acc d -> acc ||| (d `atop` strutX maxWidth)) mempty orbs
  putStrLn $ "Rendering to SVG " ++ show outFile
  renderSVG outFile (Height flags_height) total

loadOrbs fp = do
  engs <- loadEnergies fp
  let os = takeOrbs flags_first flags_total engs
      h  = text fp # fontSize (Local 0.2) `atop` strutY 0.5
  return $ h === plotOrbs os

