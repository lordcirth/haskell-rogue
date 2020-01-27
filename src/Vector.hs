module Vector where

newtype Vec2D = Vec2D (Int, Int)

vecAdd :: Vec2D -> Vec2D -> Vec2D
vecAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- get a unit vector (rounded heavily into Ints)
vecUnit :: Vec2D -> Vec2D
vecUnit (x, y) = (x*magnitude, y*magnitude)
  where
    magnitude = sqrt (x^2 +y^2)
