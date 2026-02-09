{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module GameData (module GameData) where

import Data.Foldable (find)
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.Gloss (Picture)
import Graphics.Gloss.Data.Color
import System.Random (StdGen)

import GameConstants
import GameMath

-- ================= Common type aliases =================

type Name = String
type Radius = Float
type Damage = Float

-- =========================================================
-- ================ Class constraints ======================
-- =========================================================

-- ================= Quantitative =========================

type CUR = Float
type MAX = Float

data QuantitativeType = Time CUR MAX | Fuel CUR MAX | Defence CUR MAX deriving (Eq)

putInTime :: Float -> QuantitativeType
putInTime time = Time time time

class Quantitative obj where
    quantitativeName :: obj -> String
    quantitativeCurrent :: obj -> Float
    quantitativeMax :: obj -> Float

showQuantitative :: Float -> String
showQuantitative num = show $ (round num :: Integer)

quantitativeParts :: QuantitativeType -> (CUR, MAX)
quantitativeParts (Time t t') = (t, t')
quantitativeParts (Fuel f f') = (f, f')
quantitativeParts (Defence d d') = (d, d')

instance Quantitative QuantitativeType where
    quantitativeName (Time _ _) = "TIME"
    quantitativeName (Fuel _ _) = "FUEL"
    quantitativeName (Defence _ _) = "DEF"
    quantitativeCurrent = fst . quantitativeParts
    quantitativeMax = snd . quantitativeParts

instance Quantitative Ship where
    quantitativeName _ = ""
    quantitativeCurrent = travaledDistance
    quantitativeMax = totalDistance

-- ================== Selectable ==========================

data (Selectable a) => SelectableType a = SelectableType (Maybe a) (Maybe a) deriving (Eq, Show)

class (Eq obj) => Selectable obj where
    toSelectableType :: Maybe World -> obj -> Maybe (SelectableType obj)
    isSelected :: Maybe World -> obj -> Bool
    isSelected world obj
        | (Just (SelectableType (Just real) (Just selected))) <- toSelectableType world obj = real == selected
        | otherwise = False
    isAvailable :: Maybe World -> Maybe obj -> Bool
    getAvailableColor :: Maybe obj -> Color
    getUnAvailableColor :: Maybe obj -> Color

instance Selectable Mission where
    toSelectableType (Just (World _ _ _ _ _ (Selection selected) _ _)) mission = Just $ SelectableType selected (Just mission)
    toSelectableType _ _ = Nothing
    isAvailable _ _ = True
    getAvailableColor _ = mainColor
    getUnAvailableColor _ = mainColor

instance Selectable Route where
    toSelectableType (Just (World _ _ _ _ _ (Game _ _ _ selected_route _ _) _ _)) route = Just $ SelectableType selected_route (Just route)
    toSelectableType _ _ = Nothing
    isAvailable (Just (World (Fuel fuel' _) _ _ _ _ _ _ _)) (Just route)
        | (Route _ (EffectFuelLoss (_, loss)) _) <- route = loss <= fuel'
        | otherwise = False
    isAvailable _ _ = False
    getAvailableColor _ = mainColor
    getUnAvailableColor _ = mainColor

instance Selectable PlanetConnection where
    toSelectableType (Just world@(World _ _ _ _ _ (Game _ planet _ route _ _) _ _)) connection = Just $ SelectableType (Just connection) selected
      where
        selected
            | (Just route') <- route = Just $ planetRouteToConnection world planet route'
            | otherwise = Nothing
    toSelectableType _ _ = Nothing
    isAvailable (Just world@(World _ _ _ _ _ (Game _ planet _ _ available_routes Normal) _ _)) (Just connection) =
        elem connection $ map (planetRouteToConnection world planet) available_routes
    isAvailable _ _ = False
    getAvailableColor _ = availableColor
    getUnAvailableColor _ = unavailableColor

-- ================== Colorable ===========================

class Colorable obj where
    getColor :: Maybe World -> obj -> Color

instance (Selectable a) => Colorable (Maybe (SelectableType a)) where
    getColor world (Just (SelectableType real selected))
        | real == selected = selectionColor
        | isAvailable world real = getAvailableColor real
        | otherwise = getUnAvailableColor real
    getColor _ _ = mainColor

getSelectableColor :: (Selectable obj) => Maybe World -> obj -> Color
getSelectableColor world selectable = getColor world $ toSelectableType world selectable

instance Colorable Mission where
    getColor = getSelectableColor

instance Colorable Route where
    getColor = getSelectableColor

instance Colorable QuantitativeType where
    getColor _ (Time _ _) = timeColor
    getColor _ (Fuel _ _) = fuelColor
    getColor _ (Defence _ _) = defenceColor

instance Colorable Effect where
    getColor _ (EffectFuelGain) = fuelColor
    getColor _ (EffectFuelLoss _) = fuelColor
    getColor _ (EffectRepair) = repairColor
    getColor _ (EffectNone) = white

instance Colorable Planet where
    getColor w@(Just world) planet
        | Set.member planet $ planetsVisited $ stage world = white
        | otherwise = getColor w $ planetEffect planet
    getColor _ _ = white

instance Colorable PlanetConnection where
    getColor world@(Just (World _ _ _ _ _ (Game _ _ _ _ _ _) _ _)) connection = getSelectableColor world connection
    getColor _ _ = unavailableColor

-- Colors are not added as a constant because they are only needed here
instance Colorable HazardType where
    getColor _ Asteroid = orange
    getColor _ Pirates = red
    getColor _ Nebula = aquamarine
    getColor _ Radiation = yellow
    getColor _ HazardNone = black

numberCollidingHazardsChoice :: CollidingHazards -> a -> a -> a
numberCollidingHazardsChoice (CollidingHazards num) a b
    | num <= 0 = a
    | otherwise = b

instance Colorable CollidingHazards where
    getColor _ numCollidigRout = numberCollidingHazardsChoice numCollidigRout green red

-- ================== Numbered ============================

realElemIndex :: (Eq a) => [a] -> a -> Int
realElemIndex xs x = fromJust $ x `elemIndex` xs

class Numbered obj where
    getNumber :: World -> obj -> Int

instance Numbered Mission where
    getNumber world = realElemIndex (missions world)

instance Numbered Route where
    getNumber (World _ _ _ _ _ (Game _ _ _ _ available_routes _) _ _) route = realElemIndex available_routes route
    getNumber _ _ = error "Can't get route number not in game stage"

-- ================== Named ===============================

class Named obj where
    getName :: obj -> Name
    getHeight :: obj -> Float

instance Named Planet where
    getName (Planet name _ _) = name
    getHeight _ = planetRadius * 2

instance Named Hazard where
    getName (Hazard _ name _ _ _ _) = name
    getHeight (Hazard _ _ _ r _ _) = r

-- =================== Show ===============================

instance Show Planet where
    show = planetName

instance Show QuantitativeType where
    show (Time t _) = showQuantitative t ++ "s"
    show (Fuel f _) = showQuantitative f
    show (Defence d _) = showQuantitative d

instance Show Effect where
    show (EffectFuelLoss (x, y)) = "Fuel: " ++ (show $ ((round x, round y) :: (Integer, Integer)))
    show EffectFuelGain = "Fuel"
    show EffectRepair = "Repair"
    show EffectNone = ""

instance Show EndingType where
    show LostTime = "Time and tide wait for no man"
    show LostFuel = "Lost in space"
    show LostDefence = "Died from ship explosion"
    show Won = "Mission Successful"
    show _ = ""

-- ===================== Ord / Eq ===========================

instance Ord Planet where
    planet1 `compare` planet2 = planetName planet1 `compare` planetName planet2

instance Show CollidingHazards where
    show numCollidingRout@(CollidingHazards num) =
        numberCollidingHazardsChoice numCollidingRout "clear route" ((show num) ++ " hazard(s)!")

instance Eq PlanetConnection where
    (PlanetConnection p1 p2 _) == (PlanetConnection p1' p2' _) = p1 == p1' && p2 == p2'

-- =================== Hoverable ============================

toHoverType :: (Hoverable obj) => obj -> Maybe HoverableType
toHoverType obj = Just $ HoverableType (getHoverCheck obj) (getHoverData obj)

class Hoverable obj where
    getHoverCheck :: obj -> VecFloat -> VecFloat -> Bool
    getHoverData :: obj -> HoverData

instance Hoverable Planet where
    getHoverCheck (Planet _ _ _) = collidesPointsRadius planetRadius
    getHoverData (Planet name _ effect) = ["Planet: " ++ name, "Effect: " ++ show effect]

instance Hoverable PlanetConnection where
    getHoverCheck (PlanetConnection (Planet _ pos1 _) (Planet _ pos2 _) _) real_pos mouse_pos = collidesLine line mouse_pos mouseRadius
      where
        line = getRealLine (Line pos1 pos2) real_pos
    getHoverData (PlanetConnection _ _ (Just (Route _ fuel' time'))) = [show fuel', show time']
    getHoverData _ = []

instance Hoverable Hazard where
    getHoverCheck hazard = collidesPointsRadius (hazardRadius hazard)
    getHoverData (Hazard hazard_type _ _ _ damage effect) = [show hazard_type, "Damage: " ++ show damage, show effect]

-- =========================================================
-- =================== Data types ==========================
-- =========================================================

-- ==================== World ==============================

type Transformation = Float -> Float
type PlanetConnections = Map.Map Planet [Route]
type NewIndexGen = Int -> Int
type SelectionChanger = NewIndexGen -> World -> World
type WorldTransform = World -> World
data CollidingHazards = CollidingHazards Integer deriving (Eq)

data World
    = World
    { fuel :: QuantitativeType
    , defence :: QuantitativeType
    , connections :: PlanetConnections
    , missions :: [Mission]
    , hazards :: [Hazard]
    , stage :: Stage
    , mousePos :: VecFloat
    , randomGen :: StdGen
    }
    deriving (Eq, Show)

data Effect = EffectFuelGain | EffectRepair | EffectNone | EffectFuelLoss VecFloat deriving (Eq)

data Planet = Planet {planetName :: Name, planetPos :: VecFloat, planetEffect :: Effect} deriving (Eq)
planetByName :: Name -> World -> Planet
planetByName name world = fromJust $ find ((== name) . planetName) (planets world)

data HazardType = Asteroid | Pirates | Nebula | Radiation | HazardNone deriving (Eq, Show)

data Hazard = Hazard {hazardType :: HazardType, hazardName :: Name, hazardPos :: VecFloat, hazardRadius :: Radius, hazardDamage :: Damage, hazardFuelLoss :: Effect} deriving (Eq, Show)

data PlanetConnection = PlanetConnection Planet Planet (Maybe Route) deriving (Show)
planetRouteToConnection :: World -> Planet -> Route -> PlanetConnection
planetRouteToConnection world planet route@(Route name _ _) = PlanetConnection planet (planetByName name world) (Just route)

data Mission = Mission {missionName :: Name, missionFrom :: Name, missionTo :: Name, missionTime :: QuantitativeType} deriving (Eq, Show)

data Route = Route {destinationName :: Name, routeFuelRange :: Effect, routeTime :: QuantitativeType} deriving (Eq, Show)

resetFuel, resetDefence :: World -> World
resetFuel world = world{fuel = Fuel maxFuel maxFuel}
resetDefence world = world{defence = Defence maxDefence maxDefence}

resetBaseWorld :: World -> World
resetBaseWorld = resetFuel . resetDefence

planets :: World -> [Planet]
planets world = Map.keys (connections world)

planetRoutes :: Planet -> World -> [Route]
planetRoutes planet world = Map.findWithDefault [] planet (connections world)

putGameStateInWorld :: World -> GameState -> World
putGameStateInWorld world@(World _ _ _ _ _ stage'@(Game _ _ _ _ _ _) _ _) state' = world{stage = stage'{gameState = state'}}
putGameStateInWorld world _ = world

data Stage
    = Selection (Maybe Mission)
    | Game
        { gameMission :: Mission
        , gamePlanet :: Planet
        , planetsVisited :: Set Planet
        , selectedRoute :: Maybe Route
        , gameAvailableRoutes :: [Route]
        , gameState :: GameState
        }
    | Ending EndingType Mission
    deriving (Eq, Show)

data GameState
    = PlanetTransition {ship :: Ship, appliedHazards :: [Hazard], transitionTime :: QuantitativeType, transitionFuel :: QuantitativeType}
    | Normal
    deriving (Eq, Show)
data EndingType = Won | LostTime | LostFuel | LostDefence | None deriving (Eq)

getTransitionTime :: World -> Float
getTransitionTime (World _ _ _ _ _ (Game _ _ _ _ _ (PlanetTransition _ _ (Time cur_t _) _)) _ _) = cur_t
getTransitionTime _ = 0

data Ship = Ship {shipPos :: VecFloat, shipDestination :: VecFloat, travaledDistance :: Float, totalDistance :: Float} deriving (Eq, Show)

-- ================= InterfaceElement =======================

-- given real position on screen and mouse position returns whether object is howered
type Dimensions = VecFloat
type HoverCheck = VecFloat -> VecFloat -> Bool
type HoverData = [String]

data AlignmentX = CenterX | Left | Right
data AlignmentY = CenterY | Top | Bottom

data HoverableType = HoverableType HoverCheck HoverData

data InterfaceElement = InterfaceElement
    { dimensions :: Dimensions
    , interfacePosition :: VecFloat
    , gamePosition :: Maybe VecFloat
    , interfacePicture :: Picture
    , interfacePadding :: VecFloat
    , hover :: Maybe HoverableType
    }
width, height :: InterfaceElement -> Float
width (InterfaceElement (width', _) _ _ _ _ _) = width'
height (InterfaceElement (_, height') _ _ _ _ _) = height'
setPadding :: VecFloat -> InterfaceElement -> InterfaceElement
setPadding pad element = element{interfacePadding = pad}
setPaddingZero :: InterfaceElement -> InterfaceElement
setPaddingZero element = element{interfacePadding = (0, 0)}
setWidth :: Width -> InterfaceElement -> InterfaceElement
setWidth width' element = element{dimensions = (width', height element)}

gamePositionOrZero :: InterfaceElement -> VecFloat
gamePositionOrZero (InterfaceElement _ _ (Just pos) _ _ _) = pos
gamePositionOrZero _ = (0, 0)

putInterfaceElement :: Dimensions -> VecFloat -> Picture -> InterfaceElement
putInterfaceElement dim' pos' picture = InterfaceElement dim' pos' Nothing picture padding Nothing
