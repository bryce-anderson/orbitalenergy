{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import OrcaEnergy.Parsing
import OrcaEnergy.Plotting
import OrcaEnergy

import Diagrams.Prelude
import Diagrams.Backend.SVG

import System.Directory hiding (findFiles)
import System.Environment (getArgs)
import Data.String.Utils

import qualified Data.Text as T

{-
import HFlags


defineFlag "h:height" (500 :: Double) "Height of generated diagram"
defineFlag "f:first"  (4 :: Int)      "Energy levels before HOMO to plot"
defineFlag "t:total"  (8 :: Int)      "Number of orbitals to plot"
-}

data OptFlags = OptFlags { height :: Double
                         , first  :: Int
                         , total  :: Int
                         , files  :: [String] }
    deriving (Show, Eq)
    
parseArgs :: IO OptFlags 
parseArgs = do
  args <- getArgs
  let go ("height":h:xs) Nothing f t = go xs (Just $ read h) f t
      go ("first":f:xs) h Nothing t = go xs h (Just $ read f) t
      go ("total":t:xs) h f Nothing = go xs h f (Just $ read t)
      go xs (Just h) (Just f) (Just t) = return $ OptFlags h f t xs
      go _ _ _ _ = error $ "Failed to parse flags: " ++ show args
      
  go args Nothing Nothing Nothing
    
  

main :: IO ()
main = do
  flags <- parseArgs
  fnames <- case files flags of
              [] -> findFiles
              fs -> return fs 
  putStrLn $ "Attempting to plot orbitals of files " ++ show fnames
  runPlots $ flags { files = fnames }
  putStrLn "Done."

findFiles :: IO [FilePath]
findFiles =  filter (endswith ".out") `fmap` getDirectoryContents "."

outFile = "energylevels.svg"

runPlots :: OptFlags -> IO ()
runPlots (OptFlags h f t fnames) = do
  orbs <- loadOrbs f t `mapM` fnames
  let maxWidth = (maximum $ width `map` orbs) :: Double
      total = foldl (\acc d -> acc ||| (d `atop` strutX maxWidth)) mempty orbs
      sz = mkHeight h
      
  putStrLn $ "Rendering to SVG " ++ show outFile
  renderSVG outFile sz total
  
--  loadOrbs :: 
loadOrbs first total fp = do
  engs <- loadEnergies fp
  let os = takeOrbs first total engs
      h  = text fp # fontSize (local 0.2) `atop` strutY 0.5
  return $ h === plotOrbs os

