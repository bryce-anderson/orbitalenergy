
import System.FilePath
import Debug.Trace

data OrbOcc = Empty
            | Alpha
            | Beta
            | AlphaBeta
   deriving Show

data Orbital = Orbital { energy    :: Double
                       , occupancy :: OrbOcc 
                       } deriving Show

getFile :: FilePath -> IO [String]
getFile p = do
  s <- readFile p
  return $ lines s

mocLine = "  0   2.0000     -18.907623      -514.5026  "
splitLine = splitLine' ""
  where
    splitLine' ""  (' ':xs) = splitLine xs
    splitLine' acc (' ':xs) = acc:(splitLine xs)
    splitLine' acc (x:xs)   = splitLine' (acc ++ [x]) xs
    splitLine' ""  []       = []
    splitLine' acc []       = [acc]

-- Takes the file as a big string and extracts the orbitals
getOcc :: String -> [Orbital]
getOcc = go . findEng . lines
  where
    go :: [String] -> [Orbital]
    go [] = []
    go (x:xs) = case parseEngLine x of
      Just orb -> orb:go(xs)
      Nothing  -> []

    findEng :: [String] -> [String]
    findEng ("ORBITAL ENERGIES":xs) = drop 3 xs 
    findEng (_:xs)                  = findEng xs
    findEng []                      = []
    
    parseEngLine :: String -> Maybe Orbital
    parseEngLine s = if length vs == 4 then Just orbital else Nothing
      where
        vs = splitLine s
        [_, occs,_,evs] = vs 
        occI = round $ (read occs :: Float) :: Int
        occ = if occI == 0 then Empty
              else if occI == 2 then AlphaBeta
              else undefined
        evF = read evs :: Double
        orbital = Orbital evF occ 

loadEnergies :: FilePath -> IO [Orbital]
loadEnergies p = do
  s <- readFile p
  -- putStrLn $ "Debug: " ++ take 200 s
  return $ getOcc s 

takeOrb :: Int -> Int -> [Orbital] -> [Orbital]
takeOrb homom cnt orbs = take cnt $ drop (occ - homom) orbs
  where
    occ = length $ takeWhile pred orbs
    pred (Orbital _ Empty) = False
    pred _                 = True

fname = "peroxide_peroxide_orca.out"
main :: IO ()
main = do
  putStrLn "Hello"
  orbs <- loadEnergies fname
  putStrLn $ show $ takeOrb 2 4 orbs

