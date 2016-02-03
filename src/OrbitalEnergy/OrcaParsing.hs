module OrbitalEnergy.OrcaParsing (
    loadEnergies
) where 

import OrbitalEnergy
import System.FilePath
import Debug.Trace
import Data.String.Utils (splitWs, strip)
import Data.List (sort)

findEng :: [String] -> [String]
findEng ("ORBITAL ENERGIES":xs) = drop 1 xs 
findEng (_:xs)                  = findEng xs
findEng []                      = []
    
-- Takes the file as a big string and extracts the orbitals
getOccupancy :: String -> [Orbital]
getOccupancy = decide . findEng . lines
  where
    decide :: [String] -> [Orbital]
    decide (x:_:xs) 
          | strip x == "" = go Nothing xs
          | strip x == "SPIN UP ORBITALS" = let 
               alphaOrbs = go (Just Alpha) xs
               betaOrbs  = go (Just Beta) (drop toDrop xs)
               toDrop = (length alphaOrbs) + 3
               in sort $ alphaOrbs ++ betaOrbs
          | otherwise = error ("Invalid line: " ++ x)
    decide x = error ("Invalid input: " ++ show x)

    go :: Maybe Spin -> [String] -> [Orbital]
    go _ []     = []
    go s (x:xs) = case parseEngLine s x of
                    Just orb -> orb:(go s xs)
                    Nothing  -> []

    parseEngLine :: Maybe Spin -> String -> Maybe Orbital
    parseEngLine spin s = if length vs == 4 then Just orbital else Nothing
      where
        vs = splitWs s
        [_, occs,_,evs] = vs 
        occ = if round (read occs :: Float) > 0 then Occupied else EmptyOrb
        evF = read evs :: Double
        orbital = Orbital evF occ spin

loadEnergies :: FilePath -> IO [Orbital]
loadEnergies p = do
  s <- readFile p
  return $ getOccupancy s 

