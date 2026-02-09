module TestHelpFunctions (module TestHelpFunctions) where

import qualified Data.Set as Set
import Test.HUnit (Assertion, Test (TestCase, TestLabel, TestList), assertEqual)

import GameData

-- so multiple tests can be evaluated from a list
equalSingle :: (Eq a, Show a) => (a, a) -> Assertion
equalSingle (expected, got) = assertEqual "" expected got

mapListEqualSingle :: (Eq a, Show a) => [(a, a)] -> Test
mapListEqualSingle = TestList . map (TestCase . equalSingle)

noFuelLoss :: Effect
noFuelLoss = EffectFuelLoss (0, 0)

emptyPlanet :: Planet
emptyPlanet = Planet "" (0, 0) EffectNone

tupleGt0 :: (Ord a, Num a) => (a, a) -> Bool
tupleGt0 (x, y) = x > 0 && y > 0

makeLabeledTests :: (Eq a, Show a) => [(String, [(a, a)])] -> Test
makeLabeledTests = TestList . map (\(label, l) -> TestLabel label $ mapListEqualSingle l)

putInGameStage :: Mission -> Planet -> Set.Set Planet -> Maybe Route -> Stage
putInGameStage mission planet visited route = Game mission planet visited route [] Normal

-- Game
--         { gameMission :: Mission
--         , gamePlanet :: Planet
--         , planetsVisited :: Set Planet
--         , selectedRoute :: Maybe Route
--         , gameAvailableRoutes :: [Route]
--         , gameState :: GameState
--         }
