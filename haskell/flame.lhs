 # Flame fractal in Haskell

First computing the flame fractal is difficult to do in parallel.
You can take a look at [electricsheep.org](http://electricsheep.org)
to view some examples.

Here is literate Haskell.

To start your program in Haskell you have to first declare all you imports.
I personally find the number of import annoying.
Not really a language issue but more a usage issue.
For example, instead of the actual


> module Main where
> import Data.HashMap as Dict -- cabal install HashMap
> import Data.Hashable
> import Data.Maybe   as Maybe
> import Data.Word (Word8)
> -- I need to write picture files
> -- I also prefer to declare my own Pixel data type
> import Codec.Picture hiding (Pixel) -- cabal install juicyPixels FTW
> import Control.Monad
> import System.Environment (getArgs)

I would have preferred a more concise alternative such as:

~~~
import Maybe, Word, Picture, System
import HashMap as Dict
~~~

Not a big deal thought.

Now, instead of using common types like `(Int,Int)` for point,
I prefer to use the power of Haskell types to help me discover errors.
Therefore I create a type for each of the element I need.

I will need a type for 2D points, Colors, extended colors (added color with the number)
Furthermore for efficiency reason I'll use "low level" Haskell.
I will replace `data Point = P Int Int` by an unboxed strict variant.
As it is a GHC optimization it is far more verbose to declare.
We tell GHC to unbox our type using the `{-# UNPACK #-}` comment before each field.
Furthermore to make each field _strict_ we use a `!` before the type.

> -- Data types
> --  Global state
> data Global = Global {
>		  filename  :: String
>       , imgWidth  :: Int
>       , imgHeight :: Int
>       , nbPoints  :: Int }
>
> -- Real Points
> data Point = P {-# UNPACK #-} !Float
>                {-# UNPACK #-} !Float
>

A Pixel, just a position not the color.
It is mostly like a Point but with integer.
This way I won't mess up between the two representation (on screen) and
into the mathematical plane.

> data Pixel = Pixel {-# UNPACK #-} !Int
>                    {-# UNPACK #-} !Int
>                    deriving (Eq,Ord)


I need to talk a bit about the color data structure I use.
Instead of simple RGB each color coded on 8bit.
I need something a bit more complex.

Each time a point will be lighten I'll give it a color.
But some point can be lighten multiple times and in different ways.
Then each time I light the same point I add the new light color to this point.
But in order to retrieve the right brightness,
I remember how many times the point was lighten up.
Thus this strange structure for color.
Note that for drawing the image I'll use a more standard `PixelRGB8` which is
simply three `Word8` values.

> data Color = Color {-# UNPACK #-} !Int
>                    {-# UNPACK #-} !Int
>                    {-# UNPACK #-} !Int
>                    {-# UNPACK #-} !Int

My hash key will be pixel, I then need to make my Pixel data type
an instance of Hashable.

> instance Hashable Pixel where
>   hashWithSalt n (Pixel x y) = hashWithSalt n (x,y)

Now I can use `Map` with `Pixel`s as key:

> type YMap = Map Pixel Color

I want to be able to add two `Color` and
to translate to `PixelRGB8` colors which are used to make the image.


> addColor (Color r g b n) (Color r' g' b' n') =
>   Color (r+r') (g+g') (b+b') (n+n')
>
> colorFromExt :: Color -> PixelRGB8
> colorFromExt (Color r g b n) = PixelRGB8 (fromIntegral $ div r n)
>                                             (fromIntegral $ div g n)
>                                             (fromIntegral $ div b n)

In most of my project I use the solarized theme.
I generally use only a small part of these colors.
But it is simply generally better to use a basic scheme than to use hard colors.

> -- Colors from the theme solarized
> rgb :: Int -> Int -> Int -> Color
> rgb r g b = Color r g b 1
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

> -- very basic change of representation between point and pixel
> pixelFromPoint (P x y) = Pixel (round x) (round y)

Next I needed some pseudo random number generation.
I don't need real good random number generation inside the Random for now.

> -- PSEUDO RANDOM NUMBER GENERATION
> -- !!!!!!!! DONT WORK ON 32 BITS Architecture !!!!!!!
> nextint n = (a*n + c) `rem` m
>   where
>       a = 22695477
>       c = 1
>       m = 2^32
> -- generate a random sequence of length k starting with some seed
> randlist seed = iterate nextint seed
> -- END OF PSEUDO RANDOM NUMBER GENERATION

 ## The Flame Set

Let $F_i$ be a finite family of functions.
And let consider the set $S$ which is the minimal non trivial set which is
the fixed point of the union of these function.

More precisely, let F_i be transformations.
Then consider the set S such that for all i F_i(S)=S
Or equivalently :

$$ S = U_{i} F_i(S) $$

Consider the set is non trivial (understand non empty), then There is at least
one point in it.
Finding the exact set is a difficult task.
But finding an approximation can be done this way:

 Let S_0 = {x} where x is a random point in the unit square.
 Let S_1 = x_1 = F_i(x)   for a random i
 Let S_2 = x_2 = F_j(x_1) for a random j
 ...
 Let S_n = x_n = F_k(x_{n-1}) for a random k
 ...

Each S_n will be closer to S.
At each step you add another point to S_i.
Also to remove bad initialization we generally don't consider the 20th firsts
steps. And we return only {x_21,....,x_n}.

In order to find only interesting elements we much choose our F_i family wisely.
Here are standard form of the F_i:

    F_i = affine . linear_combination [variations] . affine

affine are function of the following form:

    affine (x,y) = (ax+by+c , dx+ey+f)

It correspond to a  composition of translation, rotation and scaling.

linear_combination are function of the following form:

    linear_combination [v_i] = sum p_iv_i

affine have 6 parameters,
linear_combination [x] have length [x] parameters.

And we can use many different fonctions for the variations.

Example of variations:

- (x,y) → (x,y)
- (x,y) → (sin x,sin y)
- (x,y) → (x/r^2,y/r^2)
- (x,y) → (x sin r^2 - y cos r^2, x cos r^2 + y sin r^2)
- (x,y) → ((x-y)(x+y)/r,2xy/r)

Wich are coded here:

> -- Some variations
> vs :: [Point -> Point]
> vs = [ \ (P x y) -> P x y
>      , \ (P x y) -> P (sin x) (sin y)
>      , \ (P x y) -> let r2 = x*x+y*y in P (x/r2) (y/r2)
>      , \ (P x y) -> let r2 = x*x+y*y in P (x*(sin r2) - y*(cos r2)) (x*(cos r2) + y * (sin r2))
>      , \ (P x y) -> let r = sqrt (x^2+y^2) in P ((x - y)*(x + y)/r) (2*x*y/r)
>      ]

To define affine function a standard usage is to use matrices.

> data Matrice = M Float Float Float Float Float Float
> aff :: Matrice -> Point -> Point
> aff (M a b c d e f) (P x y) = P (a*x + b*y + c) (d*x + e*y +f)

If you use the identity variation,
the following functions generate the sierpinsky set.

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

Here are the functions for the fern functions.

> fern :: [ Point -> Point ]
> fern = [ aff $ M
>           0.0  0.0  0.0
>           0.0  0.16 0.0
>         , aff $ M
>           0.85        0.04  0.0
>           (- 0.04)  0.85  1.6
>         , aff $ M
>           0.2  (- 0.26)  0.0
>           0.23  0.22       1.6
>         , aff $ M
>           (- 0.15)  0.28  0.0
>           0.26        0.24  0.44
>         ]

Also in order to zoom on all points we generally add a final transformation
which is applied to all points. It helps zoom on the fractal for example.

> -- Transformation functions
> -- translate
> trans :: (Float,Float) -> Point -> Point
> trans (tx,ty) = aff $ M 1 0 tx 0 1 ty
> -- rotate
> rot :: Float -> Point -> Point
> rot phi = aff $ M (cos phi) (sin phi) 0.0 (- (sin phi)) (cos phi) 0.0
> -- zoom
> zoom :: Float -> Point -> Point
> zoom z = aff $ M z 0 0 0 z 0

As the final function goal is to help the final rendering position,
it seems natural to add the size of the view as parameter.

> -- The final transformation to transform the final result (zoom,rotate,translate)
> final :: Int -> Point -> Point
> final width = trans (w/2,w/2) . zoom (w/10)  . rot (- pi)
>               where w = fromIntegral width

And now the F_i functions.
As we can see, it is not only a list of functions.
But we add informations to each function:
- a probability to be used,
- a color.

> -- F_i
> fs :: [((Int, Color), Point -> Point)]
> fs = [ ((  1,   red), (vs !! 0) . (fern !! 0))
>      , (( 86, green), (vs !! 0) . (fern !! 1))
>      , (( 95,  blue), (vs !! 0) . (fern !! 2))
>      , ((100,yellow), (vs !! 0) . (fern !! 3))
>      ]

Up until now it was only a verbose babbling.
Here is the heart of our program.
Where the interesting stuff is going on.
For now, this is a rather naive implementation.
Naive in the sense that it doesn't use Monad as helper.

> flameset :: Int -> Point -> [Int] -> YMap -> YMap
> flameset w startpoint rands tmpres =
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
>         newvalue = addColor col (Maybe.fromMaybe black oldvalue)
>         -- update the dict
>         newtmpres = Dict.insert savepoint newvalue tmpres
>       in
>         flameset w newpoint (tail rands) newtmpres

The flame function is just a call to the flameset function with initial values.
Clearly there is something to be done here.

> flame :: Int -> Int -> YMap
> flame w n = flameset w (P 0.13 0.47) (take n $ randlist 0) Dict.empty

A function to read the command line arguments.

> initGlobalParams args =
>   Global { filename  = args !! 0
>		   , imgWidth  = read (args !! 1)
>          , imgHeight = read (args !! 2)
>          , nbPoints  = read (args !! 3) }

The functions needed to transform the dictionary as a picture file.

> imageFromDict :: YMap -> Int -> Int -> Image PixelRGB8
> imageFromDict dict width height = generateImage colorOfPoint width height
>   where
>     colorOfPoint :: Int -> Int -> PixelRGB8
>     colorOfPoint x y = colorFromExt $
>							fromMaybe base03 -- background color
>                        	          (Dict.lookup (Pixel x y) dict)
>
> writeImage :: String -> Int -> Int -> Int -> YMap -> IO ()
> writeImage filename w h n dict = writePng filename $ imageFromDict dict w h
>
> main :: IO ()
> main = do
>   args <- getArgs
>   if (length args<4)
>     then print $ "Usage flame ficname w h n"
>     else do
>       env  <- return (initGlobalParams args)
>       fic <- return (filename env)
>       w <- return (imgWidth env)
>       h <- return (imgHeight env)
>       n <- return (nbPoints env)
>       writeImage fic w h n (flame w n)
