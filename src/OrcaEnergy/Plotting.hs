{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module OrcaEnergy.Plotting (
    alpha,
    beta,
    alphaBeta,
    emptyLevel 
) where 

import Diagrams.Coordinates
import Diagrams.Prelude
import Diagrams.Util



-- alpha :: Renderable (Path V2 Double) b => Diagram R2
alpha = eLevel <> eArrow # lc red

-- beta :: Renderable (Path R2) b => Diagram b R2
beta = eLevel <> eArrow # rotate (1/2 @@ turn) # lc blue

-- alphaBeta :: Renderable (Path R2) b => Diagram b R2
alphaBeta = eLevel <> a # lc red <> b # lc blue
  where
    a = eArrow # translate (r2 (-0.1,0))
    b = a # rotate (1/2 @@ turn)

-- emptyLevel :: Renderable (Path R2) b => Diagram b R2
emptyLevel = eLevel

eLevel = (strokeLine eLine) # lw thick # translate (r2 (-0.5,0.0))
  where
    eLine = lineFromVertices [ p2 (0,0), p2 (1,0)]

-- eArrow :: (TrailLike t, Transformable t, OrderedField n) => t
eArrow = l # translate (r2 (0,-0.5)) # scale 0.5 # lw thick
  where
    l = strokeLine $ lineFromVertices pts
    pts = [p2 (0,-0.5), p2 (0,0.5), p2 (-0.15, 0.25)]

