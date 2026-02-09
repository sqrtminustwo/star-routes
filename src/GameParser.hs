module GameParser (
    parseConfigFile,
    check,
) where

import Control.Monad (when)
import Control.Monad.State as StateM
import Data.Map (findWithDefault, insert, keys)
import qualified Data.Map as Map (Map, fromList, insertWith, keys, lookup, (!))
import Data.Maybe (fromJust, listToMaybe)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import GameConstants
import GameData
import GameDataConstants
import GameMath

parseConfigFile :: String -> IO (Either ParseError World)
parseConfigFile file_path = parseFromFile (initialParser emptyWorld) file_path

initialParser :: World -> Parser World
initialParser w =
    choice $
        map
            (\f -> try $ f w)
            [ planetParse
            , routeParse
            , hazardParse
            , missionParse
            , commentParse
            , emptyLineParse
            , endOfFileParse
            ]

-- ============ Non parser help functions ==================

type ConnectionAdder = Planet -> Planet -> Route -> StateM.State PlanetConnections ()

check :: (MonadFail a) => Bool -> String -> a ()
check condition error_msg = do when condition $ do fail error_msg

addSingleConnection :: ConnectionAdder
addSingleConnection from _ route = state $ (\m -> ((), Map.insertWith (++) from [route] m))

addRightConnection :: ConnectionAdder
addRightConnection = addSingleConnection

addLeftConnection :: ConnectionAdder
addLeftConnection a b route = addSingleConnection b a route{destinationName = planetName a}

addDubleConnection :: ConnectionAdder
addDubleConnection planet1 planet2 route = do
    addRightConnection planet1 planet2 route
    addLeftConnection planet1 planet2 route

connectionTypes :: Map.Map String ConnectionAdder
connectionTypes = Map.fromList [("-->", addRightConnection), ("<--", addLeftConnection), ("<->", addDubleConnection)]

addNewPlanetConnection :: World -> String -> Name -> Name -> Route -> PlanetConnections
addNewPlanetConnection world connectionType planetName1 planetName2 route =
    snd $ runState ((connectionTypes Map.! connectionType) planet1 planet2 route) $ connections world
  where
    planet1 = planetByName planetName1 world
    planet2 = planetByName planetName2 world

-- =============== Parser help functions ==================

skipSpacesAndParse :: Parser a -> Parser a
skipSpacesAndParse parser = do _ <- spaces; parser

parseBetween :: String -> String -> Parser a -> Parser a
parseBetween start end parser = skipSpacesAndParse $ between (string start) (string end) parser

parseBetweenQuotesWithParser :: Parser String -> Parser String
parseBetweenQuotesWithParser = parseBetween quote quote

parseBetweenQuotes :: Parser String
parseBetweenQuotes = parseBetweenQuotesWithParser $ many $ noneOf quote

parseSymbol :: String -> Parser String
parseSymbol s = skipSpacesAndParse $ string s

parseListOfSymbols :: [String] -> Parser String
parseListOfSymbols l = choice $ map (try . parseSymbol) l

parseRestOfLine :: Parser String
parseRestOfLine = manyTill anyChar endOfLine

parseFloat :: Parser Float
parseFloat = do
    t <- skipSpacesAndParse $ Token.integer $ Token.makeTokenParser emptyDef
    pure $ fromInteger t

parseNamedFloat :: String -> Parser Float
parseNamedFloat name = do _ <- parseSymbol name; parseFloat

parseTupleValues :: String -> Parser VecFloat
parseTupleValues delimiter = do
    x <- parseFloat
    _ <- parseSymbol delimiter
    y <- parseFloat
    pure (x, y)

parseTuple :: Parser VecFloat
parseTuple = do
    _ <- parseSymbol tupleDelimiterStart
    tuple <- parseTupleValues ","
    _ <- parseSymbol tupleDelimiterEnd
    pure tuple

parseKnownPlanetName :: World -> Parser String
parseKnownPlanetName world = do
    name <- parseBetweenQuotes
    check (all (\(Planet planet_name _ _) -> name /= planet_name) $ keys $ connections world) $
        "trying to use unknown planet " ++ quote ++ name ++ quote
    pure name

-- =============== Config parse functions ==================

planetParse :: World -> Parser World
planetParse world = do
    _ <- parseSymbol "planet"
    name <- parseBetweenQuotes
    _ <- parseSymbol "at"
    coords <- parseTuple
    let effectTypeParse = do
            _ <- parseSymbol "type"
            effectType <- parseListOfSymbols $ keys possibleEffectTypes
            pure $ fromJust $ Map.lookup effectType possibleEffectTypes
    effectType <- option EffectNone (try $ effectTypeParse)
    initialParser $ world{connections = insert (Planet name coords effectType) [] $ connections world}

routeParse :: World -> Parser World
routeParse world = do
    _ <- string "route"
    start <- parseKnownPlanetName world
    connection <- parseListOfSymbols $ Map.keys connectionTypes
    end <- parseKnownPlanetName world
    _ <- parseSymbol "fuel"
    fuel' <- parseTupleValues "-"
    time <- option 2 $ parseNamedFloat "time"
    let route = Route end (EffectFuelLoss fuel') (putInTime time)
    initialParser world{connections = addNewPlanetConnection world connection start end route}

parseOptionalOrRequired :: Bool -> a -> Parser a -> Parser a
parseOptionalOrRequired required def_value parser
    | required = do parser
    | otherwise = option def_value (try $ parser)

hazardParse :: World -> Parser World
hazardParse world = do
    _ <- string "hazard"
    hazardType' <- parseListOfSymbols $ keys possibleHazardTypes
    name <- parseBetweenQuotes
    _ <- parseSymbol "at"
    coords <- parseTuple
    radius <- parseNamedFloat "radius"
    let conditinal l name' = parseOptionalOrRequired (hazardType' `elem` l) 0 (parseNamedFloat name')
    damage <- conditinal damageHazards "damage"
    fuelLoss <- conditinal fuelHazards "fuelLoss"
    let hazard = Hazard (findWithDefault HazardNone hazardType' possibleHazardTypes) name coords radius damage (EffectFuelLoss (putInVec fuelLoss))
    initialParser world{hazards = hazard : (hazards world)}

missionParse :: World -> Parser World
missionParse world = do
    _ <- string "mission"
    name <- parseBetweenQuotes
    _ <- parseSymbol "from"
    from <- parseKnownPlanetName world
    _ <- parseSymbol "to"
    to <- parseKnownPlanetName world
    timeLimit <- parseNamedFloat "timeLimit"
    initialParser world{missions = (Mission name from to (putInTime timeLimit)) : missions world}

emptyLineParse :: World -> Parser World
emptyLineParse world = do _ <- endOfLine; initialParser world

commentParse :: World -> Parser World
commentParse world = do _ <- string comment; _ <- parseRestOfLine; initialParser world

-- All after parse actions are done here
endOfFileParse :: World -> Parser World
endOfFileParse world = do
    _ <- eof
    let missions' = reverse $ missions world
    pure world{missions = missions', stage = Selection (listToMaybe missions')}
