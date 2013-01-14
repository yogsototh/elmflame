module Main where
-- import Data.HashMap as Dict -- cabal install HashMap
-- import Data.Hashable
-- import Data.Maybe   as Maybe
-- import Data.Word (Word8)
-- -- I need to write picture files
-- -- I also prefer to declare my own Pixel data type
-- import Codec.Picture hiding (Pixel) -- cabal install juicyPixels FTW
-- import Control.Monad
-- import System.Environment (getArgs)
-- -- Data types

-- Real Points
data Point = P Float Float

data Pixel = Pixel Int Int

data Color = Color Int Int Int Int

neg x = 0-x

-- instance Hashable Pixel where
--   hashWithSalt n (Pixel x y) = hashWithSalt n (x,y)
type YMap = Dict Pixel Color
addColor (Color r g b n) (Color r' g' b' n') =
  Color (r+r') (g+g') (b+b') (n+n')

fromIntegral x = x

-- colorFromExt :: Color -> PixelRGB8
colorFromExt (Color r g b n) = rgb (fromIntegral $ div r n)
                                   (fromIntegral $ div g n)
                                   (fromIntegral $ div b n)
-- Colors from the theme solarized
-- rgb :: Int -> Int -> Int -> Color
rgb r g b = Color r g b 1
black   = rgb 0   0  0
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
-- very basic change of representation between point and pixel
pixelFromPoint (P x y) = Pixel (round x) (round y)
-- PSEUDO RANDOM NUMBER GENERATION
-- !!!!!!!! DONT WORK ON 32 BITS Architecture !!!!!!!
nextint n =
  let
      a = 22695477
      c = 1
      m = 2^32
  in
   (a*n + c) `rem` m
-- generate a random sequence of length k starting with some seed
randlist seed k  = let ns = nextint seed
                   in if (k==0)
                      then []
                      else  ns:randlist ns (k-1)
-- END OF PSEUDO RANDOM NUMBER GENERATION
-- Some variations
-- vs :: [Point -> Point]
vs = [ \ (P x y) -> P x y
     , \ (P x y) -> P (sin x) (sin y)
     , \ (P x y) -> let r2 = x*x+y*y in P (x/r2) (y/r2)
     , \ (P x y) -> let r2 = x*x+y*y in P (x*(sin r2) - y*(cos r2)) (x*(cos r2) + y * (sin r2))
     , \ (P x y) -> let r = sqrt (x^2+y^2) in P ((x - y)*(x + y)/r) (2*x*y/r)
     ]
data Matrice = M Float Float Float Float Float Float
-- aff :: Matrice -> Point -> Point
aff (M a b c d e f) (P x y) = P (a*x + b*y + c) (d*x + e*y +f)
-- Some affine functions to generate the sierpinsky set
-- Equivalent to
-- sierp = [ \(x,y)->(x/2,y/2)
--         , \(x,y)->((x+1)/2,y/2)
--         , \(x,y)->(x/2,(y+1)/2) ]
-- sierp :: [ Point -> Point ]
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
-- fern :: [ Point -> Point ]
fern = [ aff $ M
          0.0  0.0  0.0
          0.0  0.16 0.0
        , aff $ M
          0.85        0.04  0.0
          (neg 0.04)  0.85  1.6
        , aff $ M
          0.2  (neg 0.26)  0.0
          0.23  0.22       1.6
        , aff $ M
          (neg 0.15)  0.28  0.0
          0.26        0.24  0.44
        ]
-- Transformation functions
-- translate
-- trans :: (Float,Float) -> Point -> Point
trans (tx,ty) = aff $ M 1 0 tx 0 1 ty
-- rotate
-- rot :: Float -> Point -> Point
rot phi = aff $ M (cos phi) (sin phi) 0.0 (neg (sin phi)) (cos phi) 0.0
-- zoom
-- zoom :: Float -> Point -> Point
zoom z = aff $ M z 0 0 0 z 0
-- The final transformation to transform the final result (zoom,rotate,translate)
-- final :: Int -> Point -> Point
final width = let w = fromIntegral width
              in trans (w/2,w) . zoom (w/10)  . rot (neg pi)

l !! i = if (i==0) then head l else (tail l) !! (i-1)

-- F_i
-- fs :: [((Int, Color), Point -> Point)]
fs = [ ((  1,   red), (vs !! 0) . (fern !! 0))
     , (( 86, green), (vs !! 0) . (fern !! 1))
     , (( 95,  blue), (vs !! 0) . (fern !! 2))
     , ((100,yellow), (vs !! 0) . (fern !! 3))
     ]

-- dropWhile :: (a -> Bool) -> [a] -> [a]

dropWhile f l = if l == []
                then []
                else if f (head l)
                     then dropWhile f (tail l)
                     else l

-- flameset :: Int -> Point -> [Int] -> YMap -> YMap
flameset w startpoint rands tmpres =
  if rands == []
    then tmpres
    else
      let
        -- take a pseudo random value
        randval = (head rands) `rem` 100
        searchfunc = (\x -> x < randval) . fst . fst
        selected = head $ dropWhile searchfunc fs
        f   = snd selected
        col = snd . fst $ selected
        -- compute the new point using a random F
        newpoint = f startpoint
        -- Now apply a final transformation and save the pixel
        savepoint = pixelFromPoint ( final w newpoint )
        -- Search the old color
        oldvalue = Dict.lookup savepoint tmpres
        -- Set the new color.
        newvalue = addColor col (Maybe.fromMaybe black oldvalue)
        -- update the dict
        newtmpres = Dict.insert savepoint newvalue tmpres
      in
        flameset w newpoint (tail rands) newtmpres
-- flame :: Int -> Int -> YMap
flame w n = flameset w (P 0.13 0.47) (randlist 0 n) Dict.empty

-- imageFromDict :: YMap -> Int -> Int -> Image PixelRGB8
-- imageFromDict dict width height = generateImage colorOfPoint width height
--   where
--     colorOfPoint :: Int -> Int -> PixelRGB8
--     colorOfPoint x y = colorFromExt $
--                           fromMaybe base03 -- background color
--                                      (Dict.lookup (Pixel x y) dict)

-- writeImage :: String -> Int -> Int -> Int -> YMap -> IO ()
-- writeImage filename w h n dict = writePng filename $ imageFromDict dict w h

point = filled mybase . rect 1 1

colorpoint ((x,y),c) = filled (colorFromExt c) . rect 1 1 $ (x,y)

background w h = collage w h
                    [filled base03 $ rect w h (div w 2,div h 2)]
scene (x,y) (w,h) =
  layers
        [ background w h
        , collage w h $
            map colorpoint $
              -- filter inpoint $
                Dict.toList $ flame w h
        ]

main = scene (0,0) (500,500)
-- main :: IO ()
-- main = do
--   args <- getArgs
--   if (length args<4)
--     then print $ "Usage flame ficname w h n"
--     else do
--       env  <- return (initGlobalParams args)
--       fic <- return (filename env)
--       w <- return (imgWidth env)
--       h <- return (imgHeight env)
--       n <- return (nbPoints env)
--       writeImage fic w h n (flame w n)
