module Main where

import Data.HashMap as Dict
import Data.Maybe   as Maybe

import Control.Monad
import System.Environment (getArgs)
data Global = Global { imgWidth  :: Int
                     , imgHeight :: Int
                     , nbPoints  :: Int }

-- Basic functions

-- -- nth element of a list
-- l !! n = if n == 0 then head l else (tail l) !! (n-1)
-- negative of a number (I don't like writing 0-x).
neg x = 0-x

rgb :: Int -> Int -> Int -> (Int,Int,Int)
rgb = (,,)
rgba = (,,,)
-- Colors (theme is solarized)
base03  = rgb 0   43  54
base02  = rgb 7   54  66
base01  = rgb 88  110 117
base00  = rgb 101 123 131
base0   = rgb 131 148 150
base1   = rgb 147 161 161
base2   = rgb 238 232 213
base3   = rgb 253 246 227
yellow  = rgb 181 137 0
orange  = rgb 203 75  22
red     = rgb 220 50  47
magenta = rgb 211 54  130
violet  = rgb 108 113 196
blue    = rgb 38  139 210
cyan    = rgb 42  161 152
green   = rgb 133 153 0

-- Default text color
mybase = rgba 108 113 196 (2/3)

-- RANDOM PART
nextint n =
    let
      a = 22695477
      c = 1
      m = 2^32
    in
     (a*n + c) `rem` m

-- generate a random sequence of length k starting with some seed
randlist seed k = if (k<0) then [] else (nextint seed):randlist (nextint seed) (k-1)

-- generate a random sequence of points of length k starting with some seed
randcouples seed k = if (k<0) then []
                     else
                       let ns = nextint seed
                           nns = nextint ns
                       in (ns,nns):randcouples nns (k-1)
-- END OF PSEUDO RANDOM NUMBER GENERATION


{-
 - Flame Set
 -
 - S = U_{i} F_i(S)
 -
 - F_i being transformations
 - General form:
 -   F = affine .  linearcomp [variation] . affine
 -   affine is a linear function (x,y) -> (ax+by+c,dx+ey+f)
 -   variation is some kind of function with some contraction properties
        ex: (x,y) -> (x,y), (sin x, sin y), etc...
 -   linearcomp [f] is a linear composition of functions: (x,y) -> Sum vi*f(x,y)
-}

data Matrice = M Float Float Float Float Float Float
aff (M a b c d e f) (x,y) = (a*x + b*y + c, d*x + e*y +f)

-- Some affine functions to generate the sierpinsky set
-- Equivalent to
-- sierp = [ \(x,y)->(x/2,y/2)
--         , \(x,y)->((x+1)/2,y/2)
--         , \(x,y)->(x/2,(y+1)/2) ]
sierp = [ aff $ M
          0.5  0.0  0.0
          0.0  0.5  0.0
        , aff $ M
          0.5  0.0  0.5
          0.0  0.5  0.0
        , aff $ M
          0.5  0.0  0.0
          0.0  0.5  0.5
        ]

-- Some variations
vs = [ \(x,y) -> (x,y)
     , \(x,y) -> (sin x, sin y)
     , \(x,y) -> let r2 = x*x+y*y in (x/r2,y/r2)
     , \(x,y) -> let r2 = x*x+y*y in (x*(sin r2) - y*(cos r2),x*(cos r2) + y * (sin r2))
     , \(x,y) -> let r = sqrt (x^2+y^2) in ((x - y)*(x + y)/r,2*x*y/r)
     ]

-- Some final functions
fs = [ (vs !! 3) . (sierp !! 0)
     , (vs !! 3) . (sierp !! 1)
     , (vs !! 3) . (sierp !! 2)]

-- Transformation functions
-- translate
trans (tx,ty) = aff $ M 1 0 tx 0 1 ty
-- rotate
rot phi = aff $ M (cos phi) (sin phi) 0.0 (neg (sin phi)) (cos phi) 0.0
-- zoom
zoom z = aff $ M z 0 0 0 z 0

-- The final transformation to transform the final result (zoom,rotate,translate)
final = trans (280,200) . zoom 300 . rot (neg 0.747)

sierpset startpoint rands tmpres =
  if rands == []
    then tmpres
    else
      let
        roundPoint (x,y) = (round x,round y)
        randval=(head rands) `rem` (length fs)
        newpoint = (fs !! randval) startpoint
        savepoint = roundPoint ( final newpoint )
        oldvalue = Dict.lookup savepoint tmpres
        newvalue = 1 + (Maybe.fromMaybe 0 oldvalue)
        newtmpres = Dict.insert savepoint newvalue tmpres
      in
        sierpset newpoint (tail rands) newtmpres

sierpinsky n = sierpset (0.0,0.0) (randlist 0 n) Dict.empty

scene env =
  let
    w = imgWidth  env
    h = imgHeight env
    n = nbPoints  env
    inpoint ((x,y),_) = x>=0 && x<=w && y>=0 && y<=h
  in (Prelude.filter inpoint) . Dict.toList $ (sierpinsky n)


initGlobalParams args =
  Global { imgWidth  = read (args !! 0)
         , imgHeight = read (args !! 1)
         , nbPoints  = read (args !! 2) }

main :: IO ()
main = do
  args <- getArgs
  if (length args<3)
    then print $ "Usage flame w h n"
    else do 
      env  <- return (initGlobalParams args)
      mapM_ (print) (scene env)
