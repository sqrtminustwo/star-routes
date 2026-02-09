module GameLogic (
    step,
    collidesConnectionHazard,
    availableRoutes,
    moveToNewPlanet,
    resetMission,
) where

import Control.Monad.State
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomR)

import GameData
import GameDataConstants
import GameMath

-- ==================== Main step functions ===========================

updateGameState :: Float -> World -> State World World
updateGameState dt (World _ _ _ _ _ (Game mission _ visited_planets (Just route) _ (PlanetTransition _ _ _ _)) _ _) = do
    _ <- updateTimeState dt
    _ <- putWorldUpdaterInState updateFuel
    _ <- putWorldUpdaterInState updateShip
    world <- putWorldUpdaterInState applyHazards
    putWorldUpdaterInState $
        ( \world' ->
            let new_world
                    | getTransitionTime world <= 0 = moveToNewPlanet (destinationName route) mission visited_planets world'
                    | otherwise = world'
             in new_world
        )
updateGameState dt (World _ _ _ _ _ (Game _ _ _ _ _ _) _ _) = updateTimeState dt
updateGameState _ world = pure world

step :: Float -> World -> IO World
step dt world = pure $ fst $ runState (updateGameState dt world) world

-- ====================== Game end check ===================================

putWorldUpdaterInState :: (World -> World) -> State World World
putWorldUpdaterInState f = state $ (\w -> let new_world = endConditionsCheck $ f w in (new_world, new_world))
updateTimeState :: Float -> State World World
updateTimeState dt = putWorldUpdaterInState $ updateTime dt

updateStage :: Bool -> Stage -> State World ()
updateStage cond new_stage =
    state $
        ( \w ->
            let new_w
                    | (Ending _ _) <- stage w = w
                    | cond = w{stage = new_stage}
                    | otherwise = w
             in ((), new_w)
        )

endConditionsCheck :: World -> World
endConditionsCheck world@(World (Fuel cur_f _) (Defence cur_d _) _ _ _ (Game mission@(Mission _ _ planet_dest (Time cur_t _)) planet _ _ _ _) _ _) = snd $ runState stateChecks world
  where
    lz num = num <= (0 :: Float)
    stateChecks = do
        updateStage (lz cur_t) (Ending LostTime mission)
        updateStage (lz $ fromInteger $ round cur_f) (Ending LostFuel mission)
        updateStage (lz cur_d) (Ending LostDefence mission)
        updateStage (planetByName planet_dest world == planet) (Ending Won mission)
endConditionsCheck w = w

updateFuel :: World -> World
updateFuel world@(World (Fuel global_cur_f global_max_f) _ _ _ _ stage'@(Game _ _ _ _ _ transition@(PlanetTransition _ _ _ fuel'@(Fuel cur_f max_f))) _ _) =
    world{fuel = Fuel new_global_f global_max_f, stage = new_stage}
  where
    fuel_lost = quantityChageWithTime fuel' transition
    new_global_f = global_cur_f - fuel_lost
    new_stage = stage'{gameState = transition{transitionFuel = Fuel (cur_f + fuel_lost) max_f}}
updateFuel world = world

updateShip :: World -> World
updateShip world@(World _ _ _ _ _ (Game _ _ _ _ _ transition@(PlanetTransition ship'@(Ship cur_pos dst_pos traveled_dst _) _ _ _)) _ _) = putGameStateInWorld world new_state
  where
    to_travel = quantityChageWithTime ship' transition
    new_pos = movePoint to_travel cur_pos dst_pos
    new_ship = ship'{shipPos = new_pos, travaledDistance = traveled_dst + to_travel}
    new_state = transition{ship = new_ship}
updateShip world = world

-- ====================== Hazards Application ==========================

putHazardInState :: Hazard -> State World ()
putHazardInState hazard = state $ (\w -> ((), applyHazard hazard w))

-- apply with HazardNone because Nebula can still have optional damage / fuel loss
applyHazard :: Hazard -> World -> World
applyHazard hazard@(Hazard Nebula _ _ _ _ _) world@(World (Fuel cur_f max_f) _ _ _ _ _ _ randomGen') =
    applyHazard hazard{hazardType = HazardNone} new_world
  where
    (fuel_loss_percentage, new_random_gen) = randomR nebulaFuelLoss randomGen'
    new_fuel = cur_f * (fuel_loss_percentage / 100)
    new_world = world{fuel = Fuel new_fuel max_f, randomGen = new_random_gen}
applyHazard (Hazard _ _ _ _ damage (EffectFuelLoss (loss, _))) world@(World (Fuel cur_f max_f) (Defence cur_d max_d) _ _ _ _ _ _) =
    world{fuel = Fuel (cur_f - loss) max_f, defence = Defence (cur_d - damage) max_d}
applyHazard _ world = world

applyHazards :: World -> World
applyHazards world@(World _ _ _ _ hazards' (Game _ _ _ _ _ transition@(PlanetTransition ship' applied_hazards _ _)) _ _) = putGameStateInWorld world_applied_hazards new_state
  where
    new_colliding_hazards_ship = filter (\hazard -> hazard `notElem` applied_hazards && collidesShipHazard ship' hazard) hazards'
    (_, world_applied_hazards) = runState (mapM putHazardInState new_colliding_hazards_ship) world
    new_applied_hazards = applied_hazards ++ new_colliding_hazards_ship
    new_state = transition{appliedHazards = new_applied_hazards}
applyHazards world = world

-- ====================== Time =============================

changeTime :: Float -> QuantitativeType -> QuantitativeType
changeTime dt (Time cur_t max_t) = Time (cur_t - dt) max_t
changeTime _ t = t

updateTime :: Float -> World -> World
updateTime dt world@(World _ _ _ _ _ stage'@(Game mission@(Mission _ _ _ mission_time) _ _ _ _ game_state) _ _) = world{stage = new_stage}
  where
    new_mission = mission{missionTime = changeTime dt mission_time}
    new_game_state
        | transition@(PlanetTransition _ _ transition_time _) <- game_state = transition{transitionTime = changeTime dt transition_time}
        | otherwise = game_state
    new_stage = stage'{gameMission = new_mission, gameState = new_game_state}
updateTime _ world = world

-- cur_t goes from max to min, so time pased is 1 - ...
quantityChageWithTime :: (Quantitative obj) => obj -> GameState -> Float
quantityChageWithTime quantitative (PlanetTransition _ _ (Time cur_t max_t) _) =
    (1 - (cur_t / max_t)) * (quantitativeMax quantitative) - (quantitativeCurrent quantitative)
quantityChageWithTime _ _ = 0

-- ================= Effects application ===================

applyEffect :: Effect -> World -> World
applyEffect EffectFuelGain world = resetFuel world
applyEffect EffectRepair world = resetDefence world
applyEffect _ world = world

applyPlanetEffect :: Planet -> Set Planet -> World -> World
applyPlanetEffect planet@(Planet _ _ effect) visited world
    | Set.notMember planet visited = applyEffect effect world
    | otherwise = world

-- ==================== Helpers ============================

collidesConnectionHazard :: PlanetConnection -> Hazard -> Bool
collidesConnectionHazard (PlanetConnection (Planet _ pos_planet1 _) (Planet _ pos_planet2 _) _) (Hazard _ _ pos_hazard r _ _) =
    collidesLine (Line pos_planet1 pos_planet2) pos_hazard r

collidesShipHazard :: Ship -> Hazard -> Bool
collidesShipHazard ship' hazard = collides
  where
    dist = distance (shipPos ship') (hazardPos hazard)
    collides = dist < (shipRadius + (hazardRadius hazard))

availableRoutes :: Planet -> World -> [Route]
availableRoutes planet world@(World _ _ _ _ _ _ _ _) = filter (isAvailable (Just world) . Just) $ planetRoutes planet world

moveToNewPlanet :: Name -> Mission -> Set Planet -> World -> World
moveToNewPlanet _ _ _ world@(World _ _ _ _ _ (Ending _ _) _ _) = world
moveToNewPlanet name mission planets_visited world =
    world_applied_effect{stage = Game mission planet new_planets_visited (listToMaybe available_routes) available_routes Normal}
  where
    planet = planetByName name world
    world_applied_effect = applyPlanetEffect planet planets_visited world
    new_planets_visited = Set.insert planet planets_visited
    available_routes = availableRoutes planet world_applied_effect

resetMission :: Mission -> World -> World
resetMission mission@(Mission _ start _ (Time _ max_t)) world = moveToNewPlanet start new_mission Set.empty reset_world
  where
    reset_world = resetBaseWorld $ world{stage = (Selection (Just mission))}
    new_mission = mission{missionTime = (Time max_t max_t)}
resetMission _ w = w
