{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Arrow (second)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Graphics.SVGFonts.ReadFont

data VarPoint = VarPoint { ptAvg :: Double
                         , ptDev :: Double
                         , ptMax :: Double
                         , ptMin :: Double
                         } deriving Show


examplePoints :: [(Double, VarPoint)]
examplePoints = [(1, VarPoint 5 1 7 2)
                ,(2, VarPoint 4 0.5 8 3)
                ,(3, VarPoint 6 2 10 1)
                ]

w = 10
h = 12

-- vertticks :: [(Double,String)] -> Dia
vertticks pairs =
    let textBits = mconcat [ text' t # alignR # moveTo ((-0.2)^&(y*h)) | (y,t) <- pairs ]
        tickBits =    mconcat [ fromVertices [ 0^&(y*h), 0.1    ^&(y*h) ] | (y,_) <- pairs ]
                   <> mconcat [ fromVertices [ w^&(y*h), (w-0.1)^&(y*h) ] | (y,_) <- pairs ]
                   <> mconcat [ fromVertices [ 0^&(y*h), w^&(y*h)       ] # lc gray # dashing [ 0.1, 0.1 ] 0 | (y,_) <- pairs ]
    in textBits <> tickBits

-- horizticks :: [(Double,String)] -> Dia
horizticks pairs =
    let textBits = mconcat [ text' t # moveTo ((x*w)^&(-0.3)) | (x,t) <- pairs ]
        tickBits =    mconcat [ fromVertices [ (x*w)^&0, (x*w)^&0.1     ] | (x,_) <- pairs ]
                   <> mconcat [ fromVertices [ (x*w)^&h, (x*w)^&(h-0.1) ] | (x,_) <- pairs ]
                   <> mconcat [ fromVertices [ (x*w)^&0, (x*w)^&h       ] # lc gray # dashing [ 0.1, 0.1 ] 0 | (x,_) <- pairs ]
    in textBits <> tickBits

showFloor = show . (floor :: Double -> Integer)
-- text' :: String -> Dia
text' s = (stroke $ textSVG' (TextOpts s lin2 INSIDE_H KERN False 0.4 0.4)) # fc black # lw 0


example = mconcat [ points
                  , horizticks (map (\x -> (x/10, showFloor x)) [0,2,4,6,8,10])
                  , vertticks (map (\y -> (y/12, showFloor y)) [0,2,4,6,8,10,12])
                  , box ]

points = position . flip zip (map buildVis examplePoints) $ map buildPts examplePoints

buildVis (x, VarPoint avg dev max min) = ((circle 0.2 # fc green) `atop` (rect 0.2 (dev*2) # fc gray)) `atop` ((p2 (x, max) ~~ p2 (x, min)) # centerXY)
buildPts (x, VarPoint avg dev max min) = (p2 (x, avg))

box = strokeLoop . closeLine . fromVertices $ [ 0^&0, 0^&h, w^&h, w^&0 ]

main = mainWith (example :: Diagram B R2)
