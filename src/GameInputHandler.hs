module GameInputHandler (
    handleIO,
) where

import Data.Maybe (listToMaybe)
import Graphics.Gloss.Interface.IO.Game
import System.Random (randomR)

import GameData
import GameLogic (resetMission)
import GameMath (distance)

-- ================== Helpers =====================

previous, next :: NewIndexGen
previous n = n - 1
next = (+ 1)

getNewSelected :: (Numbered a) => World -> NewIndexGen -> a -> [a] -> a
getNewSelected world f element elements = elements !! (f (getNumber world element) `mod` length elements)

handleArrowKeys :: SelectionChanger -> Event -> World -> World
handleArrowKeys changer (EventKey (SpecialKey KeyUp) Down _ _) world = changer previous world
handleArrowKeys changer (EventKey (SpecialKey KeyDown) Down _ _) world = changer next world
handleArrowKeys _ _ world = world

-- =============== Mission selection ==============

playCurrentSelectedMission :: World -> World
playCurrentSelectedMission world@(World _ _ _ _ _ (Selection (Just mission)) _ _) = resetMission mission world
playCurrentSelectedMission world@(World _ _ _ _ _ (Game mission _ _ _ _ _) _ _) = resetMission mission world
playCurrentSelectedMission world@(World _ _ _ _ _ (Ending _ mission) _ _) = resetMission mission world
playCurrentSelectedMission world = world

updateSelectedMission :: NewIndexGen -> World -> World
updateSelectedMission f world@(World _ _ _ missions' _ (Selection (Just current)) _ _) = world{stage = Selection $ Just $ getNewSelected world f current missions'}
updateSelectedMission _ world = world

handleSelection :: Event -> World -> World
handleSelection (EventKey (SpecialKey KeyEnter) Down _ _) world = playCurrentSelectedMission world
handleSelection e w = handleArrowKeys updateSelectedMission e w

-- ==================== Game =======================

escHandle :: World -> World
escHandle world = world{stage = Selection (listToMaybe $ missions world)}

updateSelectedRoute :: NewIndexGen -> World -> World
updateSelectedRoute f world@(World _ _ _ _ _ stage'@(Game _ _ _ (Just current) routes' _) _ _) = world{stage = stage'{selectedRoute = Just new_selected}}
  where
    new_selected = getNewSelected world f current routes'
updateSelectedRoute _ w = w

handleGame :: Event -> World -> World
-- no check needed, only available route can be selected
handleGame
    (EventKey (SpecialKey KeyEnter) Down _ _)
    world@(World _ _ _ _ _ stage'@(Game _ planet _ (Just (Route dest (EffectFuelLoss bounds) time)) _ Normal) _ randomGen') =
        world{stage = stage'{gameState = new_state}, randomGen = new_gen}
      where
        (fuel_loss, new_gen) = randomR bounds randomGen'
        ship_start = planetPos planet
        ship_end = planetPos $ planetByName dest world
        total_distance = distance ship_start ship_end
        new_state = PlanetTransition (Ship ship_start ship_end 0 total_distance) [] time (Fuel 0 fuel_loss)
handleGame e world@(World _ _ _ _ _ (Game _ _ _ _ _ Normal) _ _) = handleArrowKeys updateSelectedRoute e world
handleGame _ w = w

-- ============== Main handling functions ===========

-- explanation in GameRunner
handleIO :: Event -> World -> IO World
handleIO e w = pure $ handleNonIO e w

handleNonIO :: Event -> World -> World
handleNonIO (EventMotion pos') world = world{mousePos = pos'}
handleNonIO event world@(World _ _ _ _ _ (Selection _) _ _) = handleSelection event world
handleNonIO (EventKey (SpecialKey KeyEsc) Down _ _) world = escHandle world
handleNonIO (EventKey (Char 'r') Down _ _) world = playCurrentSelectedMission world
handleNonIO event world@(World _ _ _ _ _ (Game _ _ _ _ _ _) _ _) = handleGame event world
handleNonIO _ w = w
