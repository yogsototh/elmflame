module Flame where

import Random

-- Solarized colors
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

-- mybase = rgba 147 161 161 (2/3)
mybase = rgba 108 113 196 (2/3)

point = filled mybase . rect 1 1

background w h = filled base3 ( rect w h (div w 2,div h 2))

nextint n =
    let
      a = 22695477
      c = 1
      m = 2^32
    in
     (a*n + c) `rem` m

randlist seed k = if (k<0) then [] else (nextint seed):randlist (nextint seed) (k-1)
randcouples seed k = if (k<0) then []
                     else
                       let ns = nextint seed
                           nns = nextint ns
                       in (ns,nns):randcouples nns (k-1)

scene (x,y) (w,h) points =
  let modpoint (x,y) =  (rem x w,rem y h)
  in collage w h $
      background w h:
        map (point . modpoint) points


vs = [ \(x,y) -> (x,y)
     , \(x,y) -> (sin x, sin y)
     , \(x,y) -> let r2 = x*x+y*y in (x/r2,y/r2)
     , \(x,y) -> let r2 = x*x+y*y in (x*(sin r2) - y*(cos r2),x*(cos r2) + y * (sin r2))
     , \(x,y) -> let r = sqrt (x^2+y^2) in ((x - y)*(x + y)/r,2*x*y/r)
     ]

aff a b c d e f (x,y) = (a*x + b*y + c, d*x + e*y +f)

sierp = [ aff
          0.5  0.0  0.0
          0.0  0.5  0.0
        , aff
          0.5  0.0  0.5
          0.0  0.5  0.0
        , aff
          0.5  0.0  0.0
          0.0  0.5  0.5
        ]

-- (!!) :: [a] -> Int -> [a]
l !! n = if n == 0 then head l else (tail l) !! (n-1)

sierpset startpoint rands =
  if rands == []
    then []
    else
      let
        randval=(head rands) `rem` (length sierp)
        newpoint = (sierp !! randval) startpoint
      in
        newpoint:(sierpset newpoint (tail rands))

sierpinsky = map (\(x,y) -> (x*300,y*300)) $ drop 20 $ sierpset (0.132,0.432) (randlist 0 10000)

main = lift3 scene Mouse.position Window.dimensions (constant sierpinsky)
