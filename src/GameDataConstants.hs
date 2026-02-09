module GameDataConstants (module GameDataConstants) where

import qualified Data.Map as Map
import Graphics.Gloss
import System.Random (mkStdGen)

import GameConstants
import GameData
import GameMath

-- Constants depending on GameData
-- to preven cyclic imports

-- ================== Parser constants =====================

-- saved in map becouse we need keys for possible parse list
possibleEffectTypes :: Map.Map String Effect
possibleEffectTypes = Map.fromList [("fuel", EffectFuelGain), ("repair", EffectRepair), ("none", EffectNone)]

-- same as for effects
possibleHazardTypes :: Map.Map String HazardType
possibleHazardTypes = Map.fromList [("asteroid", Asteroid), ("pirates", Pirates), ("nebula", Nebula), ("radiation", Radiation)]

-- ================== World constants ======================

emptyWorld :: World
emptyWorld = resetBaseWorld $ World (Fuel 0 0) (Defence 0 0) Map.empty [] [] emptySelection (windowWidth, windowHeight) (mkStdGen 0)

emptySelection :: Stage
emptySelection = Selection Nothing

nebulaFuelLoss :: VecFloat
nebulaFuelLoss = (10, 50)

-- ============= InterfaceElement constants ================

screenInterface :: InterfaceElement
screenInterface = InterfaceElement (windowWidth, windowHeight) (0, 0) Nothing blank padding Nothing

emptyInterface :: InterfaceElement
emptyInterface = InterfaceElement (0, 0) (0, 0) Nothing blank padding Nothing
emptyInterfaceDim :: Dimensions -> InterfaceElement
emptyInterfaceDim dim' = emptyInterface{dimensions = dim'}

missionSelectionDimensions :: Dimensions
missionSelectionDimensions = (windowWidth * 0.7, windowHeight * 0.8)

-- frame for map
planetsFrameDimensions :: Dimensions
planetsFrameDimensions = (windowWidth * 0.70, windowHeight * 0.9)

howeredInfoDimensions :: Dimensions
howeredInfoDimensions = (x * 0.25, y * 0.15)
  where
    (x, y) = planetsFrameDimensions

-- for fitting of multiple elements, i pass list so there is no length duplication in all usages
oneElement :: [Maybe a]
oneElement = [Nothing]

shipRadius :: Radius
shipRadius = 0.5 * (fst padding)

-- triangle on the planet connection thath shows possible direction of routes between them and height of ship triangle
connectionArrowHeight, shipHeight :: Height
connectionArrowHeight = 0.5 * (fst padding)
shipHeight = shipRadius * 2
