{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}

import OrcaEnergy.OrcaParsing as OP
import qualified OrcaEnergy.GaussianParsing as GP
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

type Parser = FilePath -> IO [Orbital]

data OptFlags = OptFlags { height :: Double
                         , first  :: Int
                         , total  :: Int
                         , files  :: [String]
                         , parser :: Parser }
    
parseArgs :: IO OptFlags 
parseArgs = do
  (program:args) <- getArgs
  let prog = case program of 
               "gaussian" -> GP.loadEnergies
               "orca"     -> OP.loadEnergies
               _          -> error $ "Invalid file format: " ++ program
  
      go ("height":h:xs) Nothing f t = go xs (Just $ read h) f t
      go ("first":f:xs) h Nothing t = go xs h (Just $ read f) t
      go ("total":t:xs) h f Nothing = go xs h f (Just $ read t)
      go xs (Just h) (Just f) (Just t) = return $ OptFlags h f t xs prog
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
runPlots (OptFlags h f t fnames parser) = do
  orbs <- (\fp -> loadOrbs f t fp <$> parser fp) `mapM` fnames
  let maxWidth = (maximum $ width `map` orbs) :: Double
      total = foldl (\acc d -> acc ||| (d `atop` strutX maxWidth)) mempty orbs
      sz = mkHeight h
      
  putStrLn $ "Rendering to SVG " ++ show outFile
  renderSVG outFile sz total
  
loadOrbs first total fp engs = h === plotOrbs os
  where
    os = takeOrbs first total engs
    h  = text fp # fontSize (local 0.2) `atop` strutY 0.5

takeOrbs :: Int -> Int -> [Orbital] -> [Orbital]
takeOrbs homom cnt orbs = take cnt $ drop (occ - homom) orbs
  where
    occ = length $ takeWhile pred orbs
    pred (Orbital _ EmptyOrb _) = False
    pred _                      = True
