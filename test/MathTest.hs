module MathTest (
    mathTests,
) where

import Test.HUnit

import GameMath
import TestHelpFunctions (makeLabeledTests)

-- not labeled because each test it self is labeled
mathTests :: Test
mathTests = TestList [singleTests, vectorTests]

singleTests, vectorTests :: Test
singleTests =
    makeLabeledTests
        [
            ( "Dot products test"
            ,
                [ (0, (0, 0) `dot` (0, 0))
                , (2, (1, 1) `dot` (1, 1))
                , (677, (76, 13) `dot` (6, 17))
                ]
            )
        ,
            ( "Identity dot products test"
            ,
                [ (0, idDot (0, 0))
                , (2, idDot (1, 1))
                , (26, idDot (1, -5))
                ]
            )
        ,
            ( "Vector magnitude test"
            ,
                [ (0, mag (0, 0))
                , (sqrt 2, mag (1, 1))
                , (13, mag (5, 12))
                , (sqrt 106, mag (-5, 9))
                ]
            )
        ,
            ( "Distance test"
            ,
                [ (0, distance (0, 0) (0, 0))
                , (5, distance (0, 0) (3, 4))
                , (5, distance (6, 7) (9, 11))
                , (sqrt 5 * 7, distance (-5, 6) (2, -8))
                ]
            )
        ,
            ( "Count occurrences test"
            , map
                integerVecToFloat
                [ (0, countOccurences (1 :: Integer) [])
                , (0, countOccurences (1 :: Integer) $ [-10 .. 0] ++ [2 .. 10])
                , (3, countOccurences (2 :: Integer) [2, 1, 2, 3, 2])
                , (1, countOccurences 'a' "abc")
                ]
            )
        ]
vectorTests =
    makeLabeledTests
        [
            ( "Vector sum test"
            ,
                [ ((0, 0), vecSum (0, 0) (0, 0))
                , ((3, 5), vecSum (1, 2) (2, 3))
                , ((-1, 4), vecSum (2, 7) (-3, -3))
                ]
            )
        ,
            ( "Vector difference test"
            ,
                [ ((0, 0), vecDiff (1, 1) (1, 1))
                , ((1, 1), vecDiff (0, 0) (1, 1))
                , ((-3, 2), vecDiff (4, -1) (1, 1))
                ]
            )
        ,
            ( "Normalize test"
            ,
                [ ((0, 0), normalize (0, 0))
                , ((1, 0), normalize (5, 0))
                , (roundVec (-9 / sqrt 370, 17 / sqrt 370), roundVec $ normalize (-9, 17))
                ]
            )
        ,
            ( "Normalized direction test"
            ,
                [ ((1, 0), normDir (0, 0) (5, 0))
                , ((0, 1), normDir (2, 3) (2, 8))
                , ((-1, 0), normDir (5, 0) (0, 0))
                ]
            )
        ,
            ( "Perpendicular vector test"
            ,
                [ ((0, -1), perpendicularOfNorm (-1, 0))
                , ((-1, 0), perpendicularOfNorm (0, 1))
                , ((0, 1), perpendicularOfNorm (1, 0))
                ]
            )
        ,
            ( "Move point in normalized direction test"
            ,
                [ ((5, 0), movePointInNormDir 5 (0, 0) (1, 0))
                , ((0, -3), movePointInNormDir 3 (0, 0) (0, -1))
                , ((2, 2), movePointInNormDir 1 (1, 1) (1, 1))
                ]
            )
        ,
            ( "Middle point test"
            ,
                [ ((0, 0), middlePoint (0, 0) (0, 0))
                , ((2, 2), middlePoint (1, 1) (3, 3))
                , ((0, 1), middlePoint (-2, 1) (2, 1))
                ]
            )
        ]
  where
    roundTo :: Integer -> Float -> Float
    roundTo n x = (fromInteger $ round $ x * (10 ^ n)) / (10.0 ^^ n)
    roundVec (x, y) = (roundTo 2 x, roundTo 2 y)
