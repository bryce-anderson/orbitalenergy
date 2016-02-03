{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}

import OrbitalEnergy.OrcaParsing as OP
import qualified OrbitalEnergy.GaussianParsing as GP
import OrbitalEnergy.Plotting
import OrbitalEnergy

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

data OptFlags = OptFlags { first  :: Int
                         , total  :: Int
                         , files  :: [String]
                         , parser :: Parser }
                         
usage = "\nusage: orbitalenergy (gaussian|orca) (HOMO-N to start at) total [filenames]"

parseArgs :: IO OptFlags 
parseArgs = do
  args' <- getArgs
  let (prog,args) = case args' of 
               ("gaussian":args) -> (GP.loadEnergies,args)
               ("orca":args)     -> (OP.loadEnergies,args)
               _                 -> error $ "Invalid args: " ++ show args' ++ usage
  
      go ("first":f:xs) Nothing t = go xs (Just $ read f) t
      go ("total":t:xs) f Nothing = go xs f (Just $ read t)
      go xs (Just f) (Just t) = return $ OptFlags f t xs prog
      go _ _ _ = error $ "Failed to parse flags: " ++ show args ++ usage
  
  
  go args Nothing Nothing
    
  

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
runPlots (OptFlags f t fnames parser) = do
  orbs <- (\fp -> loadOrbs f t fp <$> parser fp) `mapM` fnames
  let maxWidth = (maximum $ width `map` orbs) :: Double
      total = foldl (\acc d -> acc ||| (d `atop` strutX maxWidth)) mempty orbs
      sz = mkHeight 1024 -- just hardcode a default value: its SVG anyway
      
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
