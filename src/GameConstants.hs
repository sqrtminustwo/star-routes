module GameConstants (module GameConstants) where

import Graphics.Gloss

-- Constants not depending on GameData
-- can be imported in GameData

-- ================== Parser constants =====================

quote :: String
quote = "\""

comment :: String
comment = "#"

tupleDelimiterStart, tupleDelimiterEnd :: String
tupleDelimiterStart = "("
tupleDelimiterEnd = ")"

fuelHazards, otherHazards, damageHazards :: [String]
damageHazards = ["asteroid", "radiation"]
fuelHazards = ["pirates"]
otherHazards = ["nebula"]

-- ================= Window-constants ======================

windowWidth, windowHeight, avgScreenWidth :: Float
windowWidth = 1920
windowHeight = 1080
avgScreenWidth = 2000

-- windowWidth = 1000
-- windowHeight = 800

-- Inital window position
windowPosX, windowPosY :: Int
windowPosX = 700
windowPosY = 200

fps :: Int
fps = 60

window :: Display
window = InWindow "Star Routes" (round windowWidth, round windowHeight) (windowPosX, windowPosY)

-- ================== Game-Constants =======================

maxFuel, maxDefence :: Float
maxFuel = 100
maxDefence = 100

background
    , mainColor
    , selectionColor
    , fuelColor
    , timeColor
    , defenceColor
    , repairColor
    , availableColor
    , unavailableColor
    , helpTextColor
    , endTextColor
    , shipColor ::
        Color
background = black
mainColor = makeColorI 160 210 255 255
selectionColor = makeColorI 11 180 195 255
fuelColor = yellow
timeColor = cyan
defenceColor = red
repairColor = green
availableColor = withAlpha 0.8 green
unavailableColor = withAlpha 0.3 red
helpTextColor = greyN 0.8
endTextColor = cyan
shipColor = mainColor

transparent :: (Color, Color)
transparent = (background, background)

planetRadius :: Float
planetRadius = windowWidth / 100

-- x / y padding for interface elements
padding :: (Float, Float)
padding = (windowWidth / 100, windowHeight / 50)

-- radius of mouse pointer
mouseRadius :: Float
mouseRadius = 10

approximateCharWidth, approximateCharHeight :: Float
approximateCharWidth = 74
approximateCharHeight = 102

-- width / height bound of frame when its being fitted
widthBound, heightBound :: Float
widthBound = windowWidth
heightBound = 100

selectHelpText, menuHelpText, mouseHelpText :: String
selectHelpText = "Arrow keys: Select | Enter: Travel"
menuHelpText = "R: Restart | Esc: Menu"
mouseHelpText = "Hover/Click for info"

-- alpha of filled color of frame
frameAlpha, selectedFrameAlpha :: Float
frameAlpha = 0.2
selectedFrameAlpha = frameAlpha * 2
