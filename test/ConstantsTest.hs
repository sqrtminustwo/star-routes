module ConstantsTest (
    constantsTests,
) where

import Test.HUnit

import GameConstants
import GameDataConstants
import TestHelpFunctions (tupleGt0)

-- constants sanity tests

constantsTests :: Test
constantsTests =
    TestList
        [ TestLabel "Constants larger than zero" singleLargerThanZero
        , TestLabel "Tuples larger than zero" tupleLargerThanZero
        , TestLabel "Assigment constants" assigmentConstants
        ]

singleLargerThanZero, tupleLargerThanZero, assigmentConstants :: Test
singleLargerThanZero =
    TestCase $
        assertBool "This set of constants should be larger than zero" $
            all
                (> 0)
                [ windowWidth
                , windowHeight
                , fromIntegral fps
                , maxFuel
                , maxDefence
                , planetRadius
                , approximateCharWidth
                , approximateCharHeight
                , widthBound
                , heightBound
                , frameAlpha
                , selectedFrameAlpha
                , shipRadius
                , connectionArrowHeight
                , shipHeight
                ]
tupleLargerThanZero =
    TestCase $
        assertBool "This set of tuples should be larger than zero" $
            all (tupleGt0) [missionSelectionDimensions, planetsFrameDimensions, howeredInfoDimensions]
assigmentConstants =
    TestList
        [ TestCase $ assertBool "Nebula fuel loss should be (10, 50)" $ nebulaFuelLoss == (10, 50)
        , TestCase $ assertBool "Max fuel should be 100" $ maxFuel == 100
        , TestCase $ assertBool "Max defence should be 100" $ maxDefence == 100
        ]
