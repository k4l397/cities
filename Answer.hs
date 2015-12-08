module Answer where

import Shape
import City
import Control.Monad.State
import Control.Monad.Random
import System.Random

mkShape :: [String] -> Shape
mkShape ["robot"]       = robot
mkShape ["tower", n]    = tower (read n)
mkShape ["city"]        = shape city
mkShape ["cubes", n, d] = cubes (read n) (read d)
mkShape args            = error ("mkShape: " ++ show args ++ " not found!")


robot :: Shape
robot =
  Stack [ -- legs
          Union [Cuboid 5  10 30, Shift 10  0 (Cuboid 5 10 30)]
          -- torso and arms
        , Union [Cuboid 20 15 10, Shift 20 10 (Cuboid 15 5 5), Shift (-15) 10 (Cuboid 15 5 5)]
          -- head
        , Cuboid 10 10 10]

tower :: Int -> Shape
tower n = Stack [Cuboid (10 * x') (10 * x') (10 * x')
                | x <- [n, n-1 .. 0]
                , let x' = fromIntegral x ]

-- TODO #1.1
cubes :: Int -> Double -> Shape
cubes n d = Union[Shift (x+d) (x+d) (Cuboid x x x),
                  Shift 0 (x+d) (Cuboid x x x),
                  Shift (x+d) 0 (Cuboid x x x),
                  (Cuboid x x x)]
  where
    x = fromIntegral n


-- TODO #2.1
cuboids :: Shape -> Int
cuboids = foldShape alg
 where
  alg = ShapeAlg empty cuboid shift stack union
  empty        = 0
  cuboid w d h = 1
  shift x y ps = ps
  stack ps     = sum ps
  union ps     = sum ps

-- TODO #2.2
height :: Shape -> Double
height = foldShape alg
  where
    alg = ShapeAlg empty cuboid shift stack union
    empty        = 0
    cuboid w d h = h
    shift x y ps = ps
    stack ps     = sum ps
    union ps     = maximum ps

-- TODO #3.1
instance Functor Bush where
  fmap = undefined

city :: City
city = fmap build land

-- TODO #3.2
build :: (Size, Pos) -> (Building, Pos)
build ((width,depth), pos) = (Cuboid (width-2) (depth-2) (10 * log (width * depth)), pos)

land :: Land
land = evalRand (plan 3 (0,0) (300, 300)) (mkStdGen 42)
