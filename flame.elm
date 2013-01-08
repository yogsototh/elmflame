module Flame where

import Random



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

mybase = rgba 147 161 161 (2/3)

point = filled mybase . rect 20 20

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

scene (x,y) (w,h) =
  let modpoint (x,y) =  (rem x w,rem y h)
  in collage w h $ background w h:map (point . modpoint) (randcouples (div y 10) (div x 10))

main = lift2 scene Mouse.position Window.dimensions
