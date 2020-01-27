module Vector where

type Vec2D = (Int, Int)

vecAdd :: Vec2D -> Vec2D -> Vec2D
vecAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- get a unit vector (rounded heavily into Ints)
vecUnit :: Vec2D -> Vec2D
vecUnit (ix, iy) = (newX, newY)
  where
    x = fromIntegral ix :: Float
    y = fromIntegral iy :: Float
    newX = round $ x/magnitude :: Int
    newY = round $ y/magnitude :: Int
    magnitude = sqrt (x^2 + y^2)
