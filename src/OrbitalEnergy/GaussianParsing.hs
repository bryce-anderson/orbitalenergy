module OrbitalEnergy.GaussianParsing (
    loadEnergies
) where 

import OrbitalEnergy
import System.FilePath
import Debug.Trace
import Data.String.Utils (splitWs, strip)
import Data.List (sort, isPrefixOf)

import Control.Applicative

loadEnergies :: FilePath -> IO [Orbital]
loadEnergies p = getOccupancy . lines <$> readFile p

getOccupancy :: [String] -> [Orbital]
getOccupancy ls = if null rst'' then sort (alphaOrbs ++ betaOrbs)
                  else trace "recursing" $ getOccupancy rst'' 
  where
    ls' = drop 1 $ dropWhile (not . isPrefixOf " The electronic state is") ls
    (alphals,rst) = break (not . isPrefixOf " Alpha") ls'
    (betals,rst') = break (not . isPrefixOf "  Beta") rst
    -- look for the last set of energies defined
    rst'' = dropWhile (not . isPrefixOf " The electronic state is") rst'
  
    alphaOrbs = concat $ parseLine (null betals) <$> alphals
    betaOrbs  = concat $ parseLine False <$> betals
  
--  Alpha  occ. eigenvalues -- -276.14886-276.14876-276.14853-276.14846 -32.28936
parseLine :: Bool -> String -> [Orbital]
parseLine isRhf l = go (words header) where
  (header,energies) = splitAt 24 l :: (String,String)
  engs = take10 $ drop 4 energies
  
  go ["Alpha","occ.","eigenvalues"]  = (mkOrb alpha Occupied . read) <$> engs
  go ["Alpha","virt.","eigenvalues"] = (mkOrb alpha EmptyOrb . read) <$> engs
  go ["Beta","occ.","eigenvalues"]   = (mkOrb beta Occupied . read)  <$> engs
  go ["Beta","virt.","eigenvalues"]  = (mkOrb beta EmptyOrb . read)  <$> engs
  
  take10 [] = []
  take10 l = h:rs where
    (h,t) = splitAt 10 l
    rs = take10 t
  
  alpha = if isRhf then Nothing else Just Alpha
  beta = Just Beta
  
  mkOrb s occ e = Orbital e occ s