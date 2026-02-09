module GameRunner (
    run,
) where

import Data.List (isSuffixOf)
import Graphics.Gloss.Interface.IO.Game
import System.Directory
import System.Environment
import System.Random (newStdGen)

import GameConstants
import GameData
import GameInputHandler
import GameLogic (step)
import GameParser
import GameRender

-- playIO is only needed for Esc key handling
run :: IO ()
run = do
    (file : _) <- getArgs
    check (null file) "no file passed"
    exists <- doesFileExist file
    check (not exists) $ file ++ " does not exist"
    check (not $ ".star" `isSuffixOf` file) $ file ++ " is not .star"
    parsed_config <- parseConfigFile file
    randomGen' <- newStdGen
    let start_game
            | (Prelude.Right world) <- parsed_config = do
                playIO window background fps world{randomGen = randomGen'} renderIO handleIO step
            | (Prelude.Left err) <- parsed_config = print err
    start_game
