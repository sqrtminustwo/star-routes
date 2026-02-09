module GameMath (module GameMath) where

import Graphics.Gloss (Color, circleSolid, color)
import Graphics.Gloss.Data.Picture (Picture)

-- ===================== Type aliases ======================

type Width = Float
type Height = Float
type X = Float
type Y = Float
type VecFloat = (X, Y)
type VecInt = (Integer, Integer)
data Line = Line VecFloat VecFloat

-- ====================== Functions =========================

floatLength :: [a] -> Float
floatLength = fromIntegral . length

mulVecX, mulVecY, mulVec :: (Fractional a) => a -> (a, a) -> (a, a)
mulVecX m (x, y) = (x * m, y)
mulVecY m (x, y) = (x, y * m)
mulVec m = mulVecX m . mulVecY m

vecSum :: VecFloat -> VecFloat -> VecFloat
vecSum (x, y) (x', y') = (x + x', y + y')

putInVec :: a -> (a, a)
putInVec e = (e, e)

fixedWidth, fixedHeight :: Float -> Float -> VecFloat
fixedWidth = (,)
fixedHeight = flip (,)

integerVecToFloat :: VecInt -> VecFloat
integerVecToFloat (x, y) = (fromIntegral x, fromIntegral y)

dot :: VecFloat -> VecFloat -> Float
dot (x, y) (x', y') = x * x' + y * y'

idDot :: VecFloat -> Float
idDot vec = dot vec vec

-- Bereken de magnitude (lengte) van een 2D vector
mag :: VecFloat -> Float
mag = sqrt . idDot

vecDiff :: VecFloat -> VecFloat -> VecFloat
vecDiff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

distance :: VecFloat -> VecFloat -> Float
distance = (mag .) . vecDiff

-- Middle point between 2 points
middlePoint :: VecFloat -> VecFloat -> VecFloat
middlePoint (x, y) (x', y') = ((x + x') / 2, (y + y') / 2)

countOccurences :: (Eq a) => a -> [a] -> Integer
countOccurences x = fromIntegral . length . filter (== x)

circleFromHeight :: Color -> Float -> Picture
circleFromHeight color' height' = color color' $ circleSolid (height' / 2)

normalize :: VecFloat -> VecFloat
normalize vec
    | len == 0 = (0, 0)
    | otherwise = mulVec (1 / len) vec
  where
    len = mag vec

-- calculate direction between 2 points and normalize it
normDir :: VecFloat -> VecFloat -> VecFloat
normDir = (normalize .) . vecDiff

-- calculate perpendicular direction of already normalized vector
perpendicularOfNorm :: VecFloat -> VecFloat
perpendicularOfNorm (x, y) = (-y, x)

-- move point given distance, point and normalized direction
movePointInNormDir :: Float -> VecFloat -> VecFloat -> VecFloat
movePointInNormDir distance' point = vecSum point . mulVec distance'

-- above function but calculate and norm direction first
movePoint :: Float -> VecFloat -> VecFloat -> VecFloat
movePoint distance' point destination = movePointInNormDir distance' point dir
  where
    dir = normDir point destination

-- draw triangle given middle point, end of line and height
triangleOn :: VecFloat -> VecFloat -> Height -> [VecFloat]
triangleOn middle_point destination height' = [apex, baseLeft, baseRight]
  where
    dir = normDir middle_point destination
    normal = perpendicularOfNorm dir
    h2 = height' / 2
    apex = movePointInNormDir h2 middle_point dir
    base_point = movePointInNormDir (-h2) middle_point dir
    base h = movePointInNormDir h base_point normal
    baseLeft = base h2
    baseRight = base (-h2)

closestPointOnLine :: Line -> VecFloat -> VecFloat
closestPointOnLine (Line line_point1 line_point2) point = closest
  where
    line_points_diff = vecDiff line_point1 line_point2
    point_line_diff = vecDiff point line_point1
    t = max 0 $ min 1 $ -(dot point_line_diff line_points_diff) / (idDot line_points_diff)
    closest = vecSum line_point1 $ mulVec t line_points_diff

-- given line that determines direction and real point, return real line
getRealLine :: Line -> VecFloat -> Line
getRealLine (Line pos1 pos2) real_pos = Line line_point1 line_point2
  where
    w2 = (distance pos1 pos2) / 2
    dir = normDir pos1 pos2
    line_point1 = movePointInNormDir w2 real_pos dir
    line_point2 = movePointInNormDir (-w2) real_pos dir

collidesPointsRadius :: Float -> VecFloat -> VecFloat -> Bool
collidesPointsRadius r p1 p2 = distance p1 p2 <= r

collidesLine :: Line -> VecFloat -> Float -> Bool
collidesLine line pos r = collidesPointsRadius r (closestPointOnLine line pos) pos
