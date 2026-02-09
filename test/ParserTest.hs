module ParserTest (
    parserTests,
) where

import Data.Either (isLeft)
import Data.List (sort)
import Data.Map (keys)
import qualified Data.Map as Map (fromList, union)
import Test.HUnit

import GameData hiding (Right)
import GameDataConstants (emptyWorld)
import GameParser
import TestHelpFunctions (equalSingle, noFuelLoss)

parserTests :: Test
parserTests =
    TestList
        [ TestLabel "Planet parser test" planetParserTest
        , TestLabel "Route parser test" routeParserTest
        , TestLabel "Hazard parser test" hazardParserTest
        , TestLabel "Mission parser test" missionParserTest
        , TestLabel "Mixed parser test" mixedParserTests
        ]

getWorldFromFile :: String -> IO World
getWorldFromFile configPath = do
    parsed <- parseConfigFile configPath
    let world
            | (Right world') <- parsed = pure world'
            | otherwise = assertFailure "Failed to parse world."
    world

equalWorldType :: (Eq a, Show a) => (World -> a) -> (String, a) -> Test
equalWorldType f (path', expected) = TestCase $ do
    world <- getWorldFromFile path'
    equalSingle (expected, f world)

parseShouldFail :: String -> Test
parseShouldFail configPath = TestCase $ do
    parsed <- parseConfigFile configPath
    assertBool ("Parse of " ++ configPath ++ " should have failed but didn't.") (isLeft parsed)

makeFailingTests :: [String] -> Test
makeFailingTests configPaths = TestList $ map (parseShouldFail) configPaths

appendToBasePath :: String -> [String] -> [String]
appendToBasePath basePath = map ((++) basePath)

makeFailingTrio :: String -> [String]
makeFailingTrio basePath = appendToBasePath basePath ["invalid1.star", "invalid2.star", "invalid3.star"]

makeFailingTrioTest :: String -> Test
makeFailingTrioTest = makeFailingTests . makeFailingTrio

makeValidPath :: String -> String
makeValidPath basePath = basePath ++ "valid.star"

planetParserTest :: Test
planetParserTest = equalWorldType (keys . connections) ("routes/test/planet.star", expected_planets)
  where
    expected_planets =
        sort
            [ Planet "Planet" (100, 100) EffectNone
            , Planet "67" (67, 67) EffectNone
            , Planet "Planet fuel" (0, 0) EffectFuelGain
            , Planet "Planet repair" (0, 0) EffectRepair
            ]

routeParserTest :: Test
routeParserTest =
    TestList $ unknown_test : connections_test
  where
    basePath = "routes/test/route/"
    unknown_test = makeFailingTrioTest basePath
    paths = appendToBasePath basePath ["valid_right.star", "valid_left.star", "valid_double.star"]
    planet_a = Planet "Starport Alpha" (0, 0) EffectFuelGain
    planet_b = Planet "Mining Colony" (300, 0) EffectNone
    route = Route (planetName planet_b) (EffectFuelLoss (20, 30)) (Time 5 5)
    connections_right = Map.fromList [(planet_a, [route])]
    connections_left = Map.fromList [(planet_b, [route{destinationName = planetName planet_a}])]
    connection_double = Map.union connections_right connections_left
    real_right = Map.union connections_right (Map.fromList [(planet_b, [])])
    real_left = Map.union connections_left (Map.fromList [(planet_a, [])])
    connections_test = map (equalWorldType connections) $ zip paths [real_right, real_left, connection_double]

hazardParserTest :: Test
hazardParserTest =
    TestList [failing_test, equalWorldType hazards (makeValidPath basePath, expected_hazards)]
  where
    basePath = "routes/test/hazard/"
    failing_test = makeFailingTrioTest basePath
    expected_hazards =
        reverse $
            [ Hazard Asteroid "Belt Alpha 1" (100, 0) 40 15 noFuelLoss
            , Hazard Asteroid "Belt Alpha 2" (100, 0) 40 15 (EffectFuelLoss (22, 22))
            , Hazard Pirates "Raider Hideout 1" (-100, 0) 50 0 (EffectFuelLoss (20, 20))
            , Hazard Pirates "Raider Hideout 2" (-100, 0) 50 15 (EffectFuelLoss (20, 20))
            , Hazard Radiation "Solar Storm 1" (300, 300) 80 25 noFuelLoss
            , Hazard Radiation "Solar Storm 2" (300, 300) 80 25 (EffectFuelLoss (30, 30))
            , Hazard Nebula "Crimson Mist 1" (-300, 300) 100 0 noFuelLoss
            , Hazard Nebula "Crimson Mist 2" (-300, 300) 100 1 noFuelLoss
            , Hazard Nebula "Crimson Mist 3" (-300, 300) 100 0 (EffectFuelLoss (2, 2))
            , Hazard Nebula "Crimson Mist 4" (-300, 300) 100 3 (EffectFuelLoss (4, 4))
            ]

missionParserTest :: Test
missionParserTest = TestList [failing_test, equalWorldType missions (makeValidPath base_path, expected_missions)]
  where
    base_path = "routes/test/mission/"
    failing_test = makeFailingTrioTest base_path
    expected_missions =
        [ Mission "Milk Run" "Starport Alpha" "Trading Post" (putInTime 30)
        , Mission "Rescue Mission" "Repair Station Beta" "Research Outpost" (putInTime 60)
        , Mission "Deep Expedition" "Lost Temple" "Deep Space Station" (putInTime 90)
        ]

mixedParserTests :: Test
mixedParserTests =
    TestList
        [ TestCase $
            do
                world_empty <- getWorldFromFile "routes/test/empty.star"
                assertBool "Empty file should result in empty world" (world_empty == emptyWorld)
        , equalWorldType id ("routes/hazards.star", world)
        ]
  where
    expected_connections =
        Map.fromList
            [
                ( Planet "Start" (0, 0) EffectNone
                ,
                    [ Route "Fuel Depot" (EffectFuelLoss (10, 20)) (putInTime 4)
                    , Route "Safe Haven" (EffectFuelLoss (20, 30)) (putInTime 5)
                    ]
                )
            , (Planet "Safe Haven" (300, 0) EffectRepair, [Route "Target" (EffectFuelLoss (10, 20)) (putInTime 4)])
            , (Planet "Fuel Depot" (150, 150) EffectFuelGain, [Route "Target" (EffectFuelLoss (15, 25)) (putInTime 4)])
            , (Planet "Target" (300, 300) EffectNone, [])
            ]
    mission = Mission "Pick Your Poison" "Start" "Target" (putInTime 50)
    expected_hazards =
        reverse $
            [ Hazard Asteroid "Rocky Belt" (150, 0) 60 30 noFuelLoss
            , Hazard Pirates "Ambush Sector" (225, 225) 50 0 (EffectFuelLoss (25, 25))
            ]
    world = emptyWorld{connections = expected_connections, missions = [mission], hazards = expected_hazards, stage = Selection (Just mission)}
