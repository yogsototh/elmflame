> module Main where
> import Data.Hashable
> import Data.HashMap as Dict -- cabal install HashMap
> import Data.Maybe   as Maybe
> import Data.Word (Word8)
> 
> import Codec.Picture -- cabal install juicyPixels FTW
> import Control.Monad
> import Control.Monad.State
> import System.Environment (getArgs)
> 
> -- Data types
> --  Global argument passed to most functions
> data Global = Global { imgWidth  :: Int
>                      , imgHeight :: Int
>                      , nbPoints  :: Int }
> 
> -- Real Points
> data Point = P {-# UNPACK #-} !Float
>                {-# UNPACK #-} !Float
> 
> data Color = Color {-# UNPACK #-} !Word8
>                    {-# UNPACK #-} !Word8
>                    {-# UNPACK #-} !Word8
> 
> data ExtColor = ExtColor {-# UNPACK #-} !Int
>                          {-# UNPACK #-} !Int
>                          {-# UNPACK #-} !Int
>                          {-# UNPACK #-} !Int
> 
> data YPixel = YPixel {-# UNPACK #-} !Int
>                      {-# UNPACK #-} !Int
>                      deriving (Eq,Ord)
> 
> instance Hashable YPixel where hashWithSalt n (YPixel x y) = hashWithSalt n (x,y)
> 
> type YMap = Map YPixel ExtColor
> 
> addExtColor (ExtColor r g b n) (ExtColor r' g' b' n') =
>   ExtColor (r+r') (g+g') (b+b') (n+n')
> 
> cmap f (Color r g b) = Color (f r) (f g) (f b)
> ecmap f (ExtColor r g b n) = ExtColor (f r) (f g) (f b) n
> 
> gammaCorrection :: Float -> Color -> Color
> gammaCorrection gamma = cmap (round . (**(1/gamma)) . fromIntegral)
> 
> colorToPixelRGB8 :: Color -> PixelRGB8
> colorToPixelRGB8 (Color r g b) = PixelRGB8 r g b
> 
> colorFromExt :: ExtColor -> Color
> colorFromExt (ExtColor r g b n) = Color (fromIntegral $ div r n)
>                                         (fromIntegral $ div g n)
>                                         (fromIntegral $ div b n)
> -- Basic functions
> neg x = 0-x
> 
> rgb :: Word8 -> Word8 -> Word8 -> Color
> rgb r g b = Color r g b
> -- Colors (theme is solarized)
> black   = rgb 0   0  0
> base03  = rgb 0   43  54
> base02  = rgb 7   54  66
> base01  = rgb 88  110 117
> base00  = rgb 101 123 131
> base0   = rgb 131 148 150
> base1   = rgb 147 161 161
> base2   = rgb 238 232 213
> base3   = rgb 253 246 227
> yellow  = rgb 181 137 0
> orange  = rgb 203 75  22
> red     = rgb 220 50  47
> magenta = rgb 211 54  130
> violet  = rgb 108 113 196
> blue    = rgb 38  139 210
> cyan    = rgb 42  161 152
> green   = rgb 133 153 0
> 
> extend :: Color -> ExtColor
> extend (Color r g b) = ExtColor (fromIntegral r) (fromIntegral g) (fromIntegral b) 1
> 
> pixelFromPoint (P x y) = YPixel (round x) (round y)
> 
> -- PSEUDO RANDOM NUMBER GENERATION
> -- !!!!!!!! DONT WORK ON 32 BITS Architecture !!!!!!!
> nextint n = (a*n + c) `rem` m
>   where
>       a = 22695477
>       c = 1
>       m = 2^32
> -- generate a random sequence of length k starting with some seed
> randlist seed n = take n $ iterate nextint seed
> -- END OF PSEUDO RANDOM NUMBER GENERATION
> 
> 
> {-
>  - Flame Set
>  -
>  - S = U_{i} F_i(S)
>  -
>  - F_i being transformations
>  - General form:
>  -   F = affine .  linearcomp [variation] . affine
>  -   affine is a linear function (x,y) -> (ax+by+c,dx+ey+f)
>  -   variation is some kind of function with some contraction properties
>         ex: (x,y) -> (x,y), (sin x, sin y), etc...
>  -   linearcomp [f] is a linear composition of functions: (x,y) -> Sum vi*f(x,y)
> -}
> 
> data Matrice = M Float Float Float Float Float Float
> aff :: Matrice -> Point -> Point
> aff (M a b c d e f) (P x y) = P (a*x + b*y + c) (d*x + e*y +f)
> 
> -- Some affine functions to generate the sierpinsky set
> -- Equivalent to
> -- sierp = [ \(x,y)->(x/2,y/2)
> --         , \(x,y)->((x+1)/2,y/2)
> --         , \(x,y)->(x/2,(y+1)/2) ]
> sierp :: [ Point -> Point ]
> sierp = [ aff $ M
>           0.5  0.0  0.0
>           0.0  0.5  0.0
>         , aff $ M
>           0.5  0.0  0.5
>           0.0  0.5  0.0
>         , aff $ M
>           0.5  0.0  0.0
>           0.0  0.5  0.5
>         ]
> fern :: [ Point -> Point ]
> fern = [ aff $ M
>           0.0  0.0  0.0
>           0.0  0.16 0.0
>         , aff $ M
>           0.85        0.04  0.0
>           (neg 0.04)  0.85  1.6
>         , aff $ M
>           0.2  (neg 0.26)  0.0
>           0.23  0.22       1.6
>         , aff $ M
>           (neg 0.15)  0.28  0.0
>           0.26        0.24  0.44
>         ]
> 
> -- Some variations
> vs :: [Point -> Point]
> vs = [ \ (P x y) -> P x y
>      , \ (P x y) -> P (sin x) (sin y)
>      , \ (P x y) -> let r2 = x*x+y*y in P (x/r2) (y/r2)
>      , \ (P x y) -> let r2 = x*x+y*y in P (x*(sin r2) - y*(cos r2)) (x*(cos r2) + y * (sin r2))
>      , \ (P x y) -> let r = sqrt (x^2+y^2) in P ((x - y)*(x + y)/r) (2*x*y/r)
>      ]
> 
> -- Some final functions
> fs :: [((Int,ExtColor),Point -> Point)]
> fs = [ ((  1,extend    red),(vs !! 0) . (fern !! 0))
>      , (( 86,extend  green),(vs !! 0) . (fern !! 1))
>      , (( 95,extend   blue),(vs !! 0) . (fern !! 2))
>      , ((100,extend yellow),(vs !! 0) . (fern !! 3))
>      ]
> 
> -- Transformation functions
> -- translate
> trans :: (Float,Float) -> Point -> Point
> trans (tx,ty) = aff $ M 1 0 tx 0 1 ty
> -- rotate
> rot :: Float -> Point -> Point
> rot phi = aff $ M (cos phi) (sin phi) 0.0 (neg (sin phi)) (cos phi) 0.0
> -- zoom
> zoom :: Float -> Point -> Point
> zoom z = aff $ M z 0 0 0 z 0
> 
> -- The final transformation to transform the final result (zoom,rotate,translate)
> final :: Int -> Point -> Point
> final width = trans (w/2,w/2) . zoom (w/10)  . rot (neg pi)
>               where w = fromIntegral width
> 
> sierpset :: Int -> Point -> [Int] -> YMap -> YMap
> sierpset w startpoint rands tmpres =
>   if rands == []
>     then tmpres
>     else
>       let
>         -- take a pseudo random value
>         randval=(head rands) `rem` 100
>         selected = head $ dropWhile ((<randval).fst.fst) fs
>         f   = snd selected
>         col = snd . fst $ selected
>         -- compute the new point using a random F
>         newpoint = f startpoint
>         -- Now apply a final transformation and save the pixel
>         savepoint = pixelFromPoint ( final w newpoint )
>         -- Search the old color
>         oldvalue = Dict.lookup savepoint tmpres
>         -- Set the new color.
>         newvalue = addExtColor col (Maybe.fromMaybe (extend black) oldvalue)
>         -- update the dict
>         newtmpres = Dict.insert savepoint newvalue tmpres
>       in
>         sierpset w newpoint (tail rands) newtmpres
> 
> sierpinsky :: Int -> Int -> YMap
> sierpinsky w n = sierpset w (P 0.13 0.47) (randlist 0 n) Dict.empty
> 
> initGlobalParams args =
>   Global { imgWidth  = read (args !! 0)
>          , imgHeight = read (args !! 1)
>          , nbPoints  = read (args !! 2) }
> 
> imageFromDict :: YMap -> Int -> Int -> Image PixelRGB8
> imageFromDict dict width height = generateImage colorOfPoint width height
>   where
>     colorOfPoint :: Int -> Int -> PixelRGB8
>     colorOfPoint x y = colorToPixelRGB8 $ colorFromExt $
>                        fromMaybe (extend base03)
>                                  (Dict.lookup (YPixel x y) dict)
> 
> writeImage :: Int -> Int -> Int -> YMap -> IO ()
> writeImage w h n dict = writePng "flame.png" $ imageFromDict dict w h
> 
> main :: IO ()
> main = do
>   args <- getArgs
>   if (length args<3)
>     then print $ "Usage flame w h n"
>     else do
>       env  <- return (initGlobalParams args)
>       w <- return (imgWidth env)
>       h <- return (imgHeight env)
>       n <- return (nbPoints env)
>       writeImage w h n (sierpinsky w n)
