module LogicTest (
    logicTests,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit
import Prelude hiding (Right)

import GameData
import GameDataConstants (emptyWorld)
import GameLogic (availableRoutes, collidesConnectionHazard, moveToNewPlanet, resetMission)
import GameMath (VecFloat)
import TestHelpFunctions

logicTests :: Test
logicTests =
    TestList
        [ TestLabel "Collides connection hazard test" collidesConnectionHazardTest
        , TestLabel "Available routes empty test" testAvailableRoutesEmpty
        , TestLabel "Available routes not empty test" testAvailableRoutesNonEmpty
        , TestLabel "Move to new planet when in ending stage test" moveToNewPlanetTestEnding
        , TestLabel "Move to new planet when not in ending stage test" moveToNewPlanetTestNotEnding
        , TestLabel "Reset mission test" resetMissionTest
        ]

putInPlanet :: VecFloat -> Planet
putInPlanet pos' = (Planet "" pos' EffectNone)
putInHazard :: VecFloat -> Radius -> Hazard
putInHazard pos' radius = (Hazard HazardNone "" pos' radius 0 EffectNone)

collidesConnectionHazardCheck :: PlanetConnection -> Hazard -> Bool -> Assertion
collidesConnectionHazardCheck connection@(PlanetConnection planet1 planet2 _) hazard@(Hazard _ _ pos' radius _ _) should_collide
    | should_collide = assertBool (error_msg True) (collidesConnectionHazard connection hazard)
    | otherwise = assertBool (error_msg False) (not $ collidesConnectionHazard connection hazard)
  where
    error_msg should =
        ( "Planet connection "
            ++ (show $ planetPos planet1)
            ++ " "
            ++ (show $ planetPos planet2)
            ++ " "
            ++ (if should then "should" else "shouldn't")
            ++ " collide with "
            ++ (show pos')
            ++ " radius = "
            ++ (show radius)
        )

collidesConnectionHazardTest :: Test
collidesConnectionHazardTest =
    TestList $
        map
            (TestCase)
            [ preset (100, 50) True
            , preset (100, 100) True
            , preset (130, 120) True
            , preset (20, 15) False
            , preset (20, 15) False
            , preset (30, 20) True
            , preset (0, 100) False
            , preset (40, 100) False
            , preset (110, 50) False
            , preset (110, 50) False
            , preset (140, 120) False
            ]
  where
    preset hazard_pos should = collidesConnectionHazardCheck (PlanetConnection (putInPlanet (100, 100)) (putInPlanet (50, 50)) Nothing) (putInHazard hazard_pos 40) should

planetNum :: Integer -> Planet
planetNum num = emptyPlanet{planetName = show num}
emptyMission :: Mission
emptyMission = Mission "" "" "" (putInTime 1)
reset :: (Float -> Float -> a) -> a
reset constructor = constructor 100 100

testAvailableRoutesEmpty, testAvailableRoutesNonEmpty :: Test
testAvailableRoutesEmpty =
    TestList $
        map
            (\(msg, fuel', connections') -> TestCase $ assertBool msg $ null $ availableRoutes (planetNum 1) emptyWorld{fuel = fuel', connections = Map.fromList connections'})
            [
                ( "World with no routes, should have no available routes"
                , reset Fuel
                , [(planetNum 1, [] :: [Route]), (emptyPlanet{planetName = "2"}, [])]
                )
            ,
                ( "World with routes but no fuel should have no available routes"
                , Fuel 0 0
                ,
                    [ (planetNum 1, [Route "2" (EffectFuelLoss (10, 10)) (putInTime 2)] :: [Route])
                    , (planetNum 2, [])
                    ]
                )
            ,
                ( "World with routes but not enough fuel should have no available routes"
                , Fuel 9 9
                ,
                    [ (planetNum 1, [Route "2" (EffectFuelLoss (10, 10)) (putInTime 2)] :: [Route])
                    , (planetNum 2, [Route "1" (EffectFuelLoss (12, 12)) (putInTime 3)])
                    ]
                )
            ]
testAvailableRoutesNonEmpty =
    TestList $
        map
            (\(msg, planet, routes, fuel', connections') -> TestCase $ assertEqual msg routes $ availableRoutes planet emptyWorld{fuel = fuel', connections = Map.fromList connections'})
            [ ( let route = Route "2" (EffectFuelLoss (6, 7)) (putInTime 1)
                 in ( "Single route test"
                    , planetNum 1
                    , [route]
                    , reset Fuel
                    , [(planetNum 1, [route]), (planetNum 2, [])]
                    )
              )
            , ( ( "2 routes but only 1 available test"
                , planetNum 1
                , [route1]
                , Fuel 15 15
                , [(planetNum 1, [route1, route2]), (planetNum 2, [route3]), (planetNum 3, [route3])]
                )
              )
            , ( let routes = [route1, route2{routeFuelRange = EffectFuelLoss (2, 2)}]
                 in ( "Multiple routes available test"
                    , planetNum 1
                    , routes
                    , Fuel 15 15
                    , [(planetNum 1, routes), (planetNum 2, [route3]), (planetNum 3, [route3])]
                    )
              )
            ]
  where
    route1 = Route "2" (EffectFuelLoss (6, 7)) (putInTime 1)
    route2 = Route "3" (EffectFuelLoss (20, 25)) (putInTime 1)
    route3 = Route "1" (EffectFuelLoss (0, 0)) (putInTime 1)

moveToNewPlanetTestEnding, moveToNewPlanetTestNotEnding :: Test
moveToNewPlanetTestEnding = TestCase $ assertEqual "Should have no effect on ended game" world $ moveToNewPlanet "1" emptyMission Set.empty world
  where
    world = emptyWorld{stage = Ending LostTime emptyMission, connections = Map.empty}
moveToNewPlanetTestNotEnding =
    TestList $
        map
            ( \(msg, move_from, move_to, before_extra, after_extra) ->
                let
                    visited = Set.singleton move_from
                    world_before =
                        before_extra $
                            emptyWorld{stage = putInGameStage emptyMission move_from visited Nothing, connections = Map.fromList [(move_from, [] :: [Route]), (move_to, [])]}
                    world_after =
                        after_extra $
                            world_before{stage = putInGameStage emptyMission move_to (Set.insert move_to visited) Nothing}
                 in
                    TestCase
                        $ assertEqual
                            msg
                            world_after
                        $ moveToNewPlanet (planetName move_to) emptyMission (planetsVisited $ stage world_before) world_before
            )
            [
                ( "Should move from one planet to other"
                , planetNum 1
                , planetNum 2
                , id
                , id
                )
            ,
                ( "Should apply fuel gain when visiting for the first time"
                , planetNum 1
                , (planetNum 2){planetEffect = EffectFuelGain}
                , (\w -> w{fuel = makeBefore Fuel})
                , (\w -> w{fuel = reset Fuel})
                )
            ,
                ( "Should apply defence gain when visiting for the first time"
                , planetNum 1
                , (planetNum 2){planetEffect = EffectRepair}
                , (\w -> w{defence = makeBefore Defence})
                , (\w -> w{defence = reset Defence})
                )
            ,
                ( "Should not apply fuel gain when visiting not for the first time"
                , planetNum 1
                , (planetNum 2){planetEffect = EffectFuelGain}
                , (\w -> w{defence = makeBefore Fuel, stage = putInGameStage emptyMission (planetNum 1) (Set.fromList [planetNum 1, planetNum 2]) Nothing})
                , (\w -> w{defence = makeBefore Fuel})
                )
            ]
  where
    makeBefore constructor = constructor 10 10

resetMissionTest :: Test
resetMissionTest =
    TestList $
        map
            ( \(time_before, defence_before, fuel_before, planet_before) ->
                let
                    world_before =
                        emptyWorld
                            { defence = defence_before
                            , fuel = fuel_before
                            , connections = Map.fromList [(planet_before, []), (mission_start, [])]
                            , stage = putInGameStage mission{missionTime = time_before} planet_before (Set.empty) Nothing
                            }
                    world_after =
                        world_before
                            { fuel = reset Fuel
                            , defence = reset Defence
                            , stage = putInGameStage mission mission_start (Set.singleton mission_start) Nothing
                            }
                 in
                    TestCase $ assertEqual "" world_after $ resetMission mission world_before
            )
            [ (putInTime 2, Defence 2 2, Fuel 3 3, planetNum 2)
            ]
  where
    mission_start = planetNum 1
    mission = Mission "Crazy Mission" (planetName mission_start) "2" (putInTime 911)
