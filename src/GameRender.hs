module GameRender (
    renderIO,
) where

import Control.Monad (msum)
import Control.Monad.State
import Graphics.Gloss hiding (Line, dim)
import Prelude hiding (Left, Right)

import GameConstants
import GameData
import GameDataConstants
import GameLogic (collidesConnectionHazard)
import GameMath

-- ===================== Type aliases ======================

type FillState = (Float, [InterfaceElement])
type FillerState = State FillState InterfaceElement
type MultiFillerState = State FillState [InterfaceElement]
type FillerFunction = InterfaceElement -> FillerState
type Translation = Float -> InterfaceElement
type FillThisFrame = Maybe (InterfaceElement -> FillerState)
type FrameFiller a = a -> InterfaceElement -> FillThisFrame -> FillerState
type FrameDataFiller a = InterfaceElement -> a -> FrameFiller a -> [InterfaceElement]
type ListDimension = [InterfaceElement] -> Float
type DimensionApply = Float -> Dimensions
type Bound = Maybe Float
type DimensionFitter = InterfaceElement -> Bound -> Float -> Float -> State FillState Float
type GameStageDrawer a = World -> a -> InterfaceElement

-- =========================================================
-- =================== Help functions ======================
-- =========================================================

-- ====================== Text =============================

-- gloss places text with its most left point at (0, 0)
putTextIntoInterface :: String -> Color -> VecFloat -> InterfaceElement
putTextIntoInterface str c pad = translate' (-width' / 2) (-height' / 2) textInterface
  where
    width' = (fromIntegral $ length str) * approximateCharWidth
    height' = approximateCharHeight
    textInterface = InterfaceElement (width', height') (0, 0) Nothing (color c $ text str) pad Nothing

putTextIntoInterfaceRelative :: String -> Color -> VecFloat -> InterfaceElement -> InterfaceElement
putTextIntoInterfaceRelative str c pad frame = smallScale $ scaleRelativeUp frame (putTextIntoInterface str c pad)

-- ============== Wrapping InterfaceElements ================

wrapIntoInterfaceMany :: InterfaceElement -> [InterfaceElement] -> InterfaceElement
wrapIntoInterfaceMany parent children = parent{interfacePicture = pictures $ map interfacePicture $ parent : children}
wrapIntoInterface :: InterfaceElement -> InterfaceElement -> InterfaceElement
wrapIntoInterface parent child = wrapIntoInterfaceMany parent [child]

-- get interface element which can wrap all of the given interface elements
getWrappingInterfaceHeight :: [InterfaceElement] -> InterfaceElement
getWrappingInterfaceHeight elements = emptyInterfaceDim (width', getTotalHeight elements)
  where
    width' = maximum $ map (\(InterfaceElement (w, _) _ _ _ (pad_x, _) _) -> w + pad_x) elements

-- help function for wrapNamed
wrapElementAndText :: InterfaceElement -> InterfaceElement -> FrameFiller (Maybe a) -> InterfaceElement
wrapElementAndText element text' filler = filled
  where
    frame = getWrappingInterfaceHeight [element, text']
    filled = wrapIntoInterface element $ fillFrameY frame Nothing filler

-- ========== Gloss functions for InterfaceElement ==========

translateX, translateY :: Float -> InterfaceElement -> InterfaceElement
translateX x' element@(InterfaceElement _ (x, y) _ picture _ _) = element{interfacePosition = (x + x', y), interfacePicture = translate x' 0 picture}
translateY y' element@(InterfaceElement _ (x, y) _ picture _ _) = element{interfacePosition = (x, y + y'), interfacePicture = translate 0 y' picture}
translate' :: X -> Y -> InterfaceElement -> InterfaceElement
translate' x y = translateX x . translateY y

scale' :: Float -> InterfaceElement -> InterfaceElement
scale' factor element@(InterfaceElement (width', height') _ _ picture _ _) = element{dimensions = (width' * factor, height' * factor), interfacePicture = scale factor factor picture}

-- given factor, parent and element
scaleRelativeUpDown :: Bool -> InterfaceElement -> InterfaceElement -> InterfaceElement
scaleRelativeUpDown scale_down (InterfaceElement (w', h') _ _ _ _ _) element@(InterfaceElement (w, h) _ _ _ _ _)
    | scale_down && w <= w' && h <= h' = element
    | otherwise = scale' (min (w' / w) (h' / h)) element

scaleRelativeDown, scaleRelativeUp :: InterfaceElement -> InterfaceElement -> InterfaceElement
scaleRelativeDown = scaleRelativeUpDown True
scaleRelativeUp = scaleRelativeUpDown False

tinyScale, smallScale, mediumScale, largeScale :: InterfaceElement -> InterfaceElement
tinyScale = scale' 0.1
smallScale = scale' 0.2
mediumScale = scale' 1.2
largeScale = scale' 1.5

-- Given whether element should be scaled, alignment, element itself and its parent, align (and scale if True) relative to its parent
alignX :: AlignmentX -> InterfaceElement -> InterfaceElement -> InterfaceElement
alignX CenterX (InterfaceElement _ (x, _) _ _ _ _) element = translateX x element
alignX Left (InterfaceElement (w', _) (x, _) _ _ _ _) element@(InterfaceElement (w, _) _ _ _ (pad_x, _) _) = translateX (x - w' / 2 + w / 2 + pad_x) element
alignX Right (InterfaceElement (w', _) (x, _) _ _ _ _) element@(InterfaceElement (w, _) _ _ _ (pad_x, _) _) = translateX (x + w' / 2 - w / 2 - pad_x) element

alignScaleX, alignNoScaleX :: AlignmentX -> InterfaceElement -> InterfaceElement -> InterfaceElement
alignScaleX alignment parent element = alignX alignment parent (scaleRelativeDown parent element)
alignNoScaleX alignment parent element = alignX alignment parent element

alignYMulti :: AlignmentY -> InterfaceElement -> [InterfaceElement] -> MultiFillerState
alignYMulti CenterY frame@(InterfaceElement (_, height') _ _ _ (_, pad_y) _) elements = do
    spacer <- spacerHeight frame $ (getTotalHeight elements / 2) + (height' / 2) - pad_y
    mapM fillY $ spacer : elements
alignYMulti Top frame@(InterfaceElement (_, h') (_, y') _ _ _ _) elements = do
    mapM (\e@(InterfaceElement (_, h) _ _ _ (_, pad_y) _) -> addWithoutFilling $ translateY (y' + h' / 2 - h / 2 - pad_y) $ alignScaleX Left frame e) elements
alignYMulti Bottom frame elements = do
    _ <- fillY frame{interfacePicture = blank}
    mapM (fillY . alignScaleX CenterX frame) elements

alignY :: AlignmentY -> InterfaceElement -> [InterfaceElement] -> FillerState
alignY alignment parent elements = do _ <- alignYMulti alignment parent elements; noFilling

-- ================= Frame filling =================

-- fill element and give it filled version back, keep dimension list of all filled elements as state
fill :: Translation -> Transformation -> FillerState
fill translation new_state_gen = state (\(s, interfaces) -> let new_interface = translation s in (new_interface, (new_state_gen s, new_interface : interfaces)))

-- translation / 2 because should translate to middle point, while state changes on full width / height
fillX, fillY :: FillerFunction
fillY element@(InterfaceElement (_, height') _ _ _ (_, pad_y) _) = fill (\y -> translateY (y - height' / 2 - pad_y) element) (\y -> y - height' - pad_y)
fillX element@(InterfaceElement (width', _) _ _ _ (pad_x, _) _) = fill (\x -> translateX (x + width' / 2 + pad_x) element) (\x -> x + width' + pad_x)

-- as we add without filling, added element should have no effect on future fitting, thats why we set its dimensions / padding to 0
addWithoutFilling :: InterfaceElement -> FillerState
addWithoutFilling element = state (\(s, interfaces) -> let element' = element{dimensions = (0, 0), interfacePadding = (0, 0)} in (element, (s, element' : interfaces)))

getFilledInterfaces :: (InterfaceElement, (Float, [InterfaceElement])) -> [InterfaceElement]
getFilledInterfaces (_, (_, interfaces)) = interfaces

-- frameAlignment so we don't have to write (fillAlignX frame $ ...) but just write (fillThisFrame $ ...)
-- not every function needs it so we pass it in a Maybe
fillFrameData :: FillState -> FillThisFrame -> InterfaceElement -> a -> FrameFiller a -> [InterfaceElement]
fillFrameData start_state frameAlignment frame element filler = getFilledInterfaces $ runState (filler element frame frameAlignment) start_state

fillFrame :: FrameDataFiller a -> InterfaceElement -> a -> FrameFiller a -> InterfaceElement
fillFrame fillerGlob frame toFillData filler = wrapIntoInterfaceMany frame $ fillerGlob frame toFillData filler

fillFrameX, fillFrameY :: InterfaceElement -> a -> FrameFiller a -> InterfaceElement
fillFrameX frame@(InterfaceElement (width', _) (x, _) _ _ _ _) = fillFrame (fillFrameData (x - width' / 2, []) Nothing) frame
fillFrameY frame@(InterfaceElement (_, height') (_, y) _ _ _ _) = fillFrame (fillFrameData (y + height' / 2, []) (Just $ fillAlignXFrame frame)) frame

-- help function to use inside filler functions
fillAlignXFrame :: InterfaceElement -> InterfaceElement -> FillerState
fillAlignXFrame = (fillY .) . alignX CenterX

fillSelectableX :: (Selectable a, Colorable a) => World -> InterfaceElement -> (World -> FrameFiller a) -> a -> InterfaceElement
fillSelectableX world frame filler selectable = fillFrameX colored_frame selectable (filler world)
  where
    alpha
        | isSelected (Just world) selectable = selectedFrameAlpha
        | otherwise = frameAlpha
    colored_frame = fillFrameWithColorAlpha alpha mainColor $ makeFrame (getColor (Just world) selectable) (dimensions frame)

makeFrameFiller :: FillerState -> FrameFiller a
makeFrameFiller s = (\_ _ _ -> s)

noFilling :: FillerState
noFilling = pure emptyInterface

-- ================= Frame fitting =================

getTotalHeight, getTotalWidth :: ListDimension
getTotalHeight = sum . map (\(InterfaceElement (_, height') _ _ _ (_, pad_y) _) -> height' + pad_y)
getTotalWidth = sum . map (\(InterfaceElement (width', _) _ _ _ (pad_x, _) _) -> width' + pad_x)

-- get dimension so that "amount_to_fit" elements fit between already added elements and "reserve"
fittingDimension :: Float -> Float -> ListDimension -> Maybe Float -> Float -> Float -> State FillState Float
fittingDimension frame_pad frame_dim interfacesDim bound amount_to_fit reserve =
    state $
        ( \cur@(_, interfaces) ->
            let
                pad = frame_pad * (amount_to_fit + 1)
                remaining = max 0 $ frame_dim - (interfacesDim interfaces) - pad - reserve
                new_dim = remaining / amount_to_fit
                new_dim_bounded
                    | (Just b) <- bound = min b new_dim
                    | otherwise = new_dim
             in
                (new_dim_bounded, cur)
        )

fittingHeight, fittingWidth :: DimensionFitter
fittingHeight (InterfaceElement (_, height') _ _ _ (_, pad_y) _) = fittingDimension pad_y height' getTotalHeight
fittingWidth (InterfaceElement (width', _) _ _ _ (pad_x, _) _) = fittingDimension pad_x width' getTotalWidth

-- given frame interface maker, bound for height/width, fitter (height/width), dimension applyer (fixed height/fixed width),
-- frame, list of elements to fit and reserved width/height
-- get fitting frame out of running state monad
fittingFramePicture :: (Dimensions -> InterfaceElement) -> Bound -> DimensionFitter -> DimensionApply -> InterfaceElement -> [a] -> Float -> FillerState
fittingFramePicture interface_maker bound fitter applyer frame elements_to_fit reserved = do
    dim' <- fitter frame bound (floatLength elements_to_fit) reserved
    pure $ interface_maker $ applyer dim'

fittingFrame :: (Color, Color) -> Bound -> DimensionFitter -> DimensionApply -> InterfaceElement -> [a] -> Float -> FillerState
fittingFrame (frame_color, fill_color) = fittingFramePicture (\dim -> fillFrameWithColor fill_color $ makeFrame frame_color dim)

fittingFrameHeightBounded, fittingFrameWidthBounded :: (Color, Color) -> Float -> InterfaceElement -> [a] -> Float -> FillerState
fittingFrameHeightBounded colors width' = fittingFrame colors (Just heightBound) fittingHeight (fixedWidth width')
fittingFrameWidthBounded colors height' = fittingFrame colors (Just widthBound) fittingWidth (fixedHeight height')

spacerHeight :: InterfaceElement -> Float -> FillerState
spacerHeight frame reserved = fittingFramePicture (\dim -> setPaddingZero $ emptyInterfaceDim dim) Nothing fittingHeight (fixedWidth 0) frame oneElement reserved

-- ======================== Other ==========================

makeDelimiterLine :: Float -> InterfaceElement -> InterfaceElement
makeDelimiterLine width_fraction (InterfaceElement (width', _) pos' _ _ _ _) =
    putInterfaceElement (w, 1) pos' (color mainColor $ line [(-w2, 0), (w2, 0)])
  where
    w = width_fraction * width'
    w2 = w / 2

makeFrame :: Color -> Dimensions -> InterfaceElement
makeFrame c dim@(width', height') = putInterfaceElement dim (0, 0) (color c $ rectangleWire width' height')

fillFrameWithColorAlpha :: Float -> Color -> InterfaceElement -> InterfaceElement
fillFrameWithColorAlpha alpha color_fill frame =
    wrapIntoInterface frame frame{interfacePicture = color (withAlpha alpha color_fill) $ rectangleSolid (width frame) (height frame)}
fillFrameWithColor :: Color -> InterfaceElement -> InterfaceElement
fillFrameWithColor = fillFrameWithColorAlpha frameAlpha

triangle :: Color -> VecFloat -> VecFloat -> Height -> Picture
triangle color' pos' destination = color color' . polygon . triangleOn pos' destination

gameElementsToInterfaceSpace :: InterfaceElement -> InterfaceElement -> [InterfaceElement] -> MultiFillerState
gameElementsToInterfaceSpace frame_unfilled frame_filled elements = do
    let
        frame_translation = vecDiff (interfacePosition frame_unfilled) (interfacePosition frame_filled)
        content = map (\e -> let (x, y) = vecSum (gamePositionOrZero e) frame_translation in translate' x y $ e) elements
    mapM addWithoutFilling content

-- =========================================================
-- ================ Mission selection render ===============
-- =========================================================

numberedCircle :: Integer -> InterfaceElement -> InterfaceElement
numberedCircle number (InterfaceElement (_, height') pos' _ _ _ _) = wrapIntoInterface circleFrame indexElement
  where
    circleFrame = scale' 0.7 $ putInterfaceElement (putInVec height') pos' (circleFromHeight mainColor height')
    indexElement = mediumScale $ putTextIntoInterfaceRelative (show number) white padding circleFrame

fillMission :: World -> Mission -> InterfaceElement -> FillThisFrame -> FillerState
fillMission world mission@(Mission name from to time) frame _ = do
    _ <- fillX $ numberedCircle ((fromIntegral $ getNumber world mission) + 1) frame
    _ <- fillX $ putTextIntoInterfaceWhite name padding
    -- getTotalWidth is called because (height timeInterface) won't count padding
    direction_frame <- fittingFrameWidthBounded transparent (height frame) frame oneElement (getTotalWidth [timeInterface])
    let putTextIntoDirectionFrame str = mediumScale $ putTextIntoInterfaceRelative str mainColor padding' direction_frame
    _ <- mapM (fillX . putTextIntoDirectionFrame) [from, "->", to]
    addWithoutFilling timeInterface
  where
    putTextIntoInterfaceWhite str pad = putTextIntoInterfaceRelative str white pad frame
    padding' = mulVecX 3 padding
    timeFrameDim = (width frame / 10, height frame * 0.8)
    timeFrame = alignX Right frame $ fillFrameWithColor mainColor $ makeFrame mainColor timeFrameDim
    timeText = alignX CenterX timeFrame $ putTextIntoInterfaceWhite (show time) padding
    timeInterface = wrapIntoInterface timeFrame timeText

fillMissionSelectionFrame :: FrameFiller World
fillMissionSelectionFrame world frame (Just fillThisFrame) = do
    _ <-
        mapM
            fillThisFrame
            [ largeScale $ putTextIntoInterfaceRelative "STAR ROUTES" yellow padding frame
            , mediumScale $ putTextIntoInterfaceRelative "SELECT MISSION" cyan (20, 50) frame
            , delimiterLine
            ]
    mission_frame <- fittingFrameHeightBounded transparent (width delimiterLine) frame (missions world) (getTotalHeight [helpText, delimiterLine])
    _ <- mapM (fillThisFrame . fillSelectableX world mission_frame fillMission) $ missions world
    lowerLine <- fillThisFrame delimiterLine
    fillY $ alignNoScaleX Right lowerLine helpText
  where
    helpText = scale' 0.1 $ putTextIntoInterface "Use arrow keys to move, press ENTER to select mission" helpTextColor padding
    delimiterLine = makeDelimiterLine 0.7 frame
fillMissionSelectionFrame _ _ _ = noFilling

-- =========================================================
-- ================= Status panel render ===================
-- =========================================================

-- fillQuantitativeBar help function
filledRectangle :: (Quantitative obj, Colorable obj) => obj -> InterfaceElement -> InterfaceElement
filledRectangle obj (InterfaceElement dim' _ _ _ _ _) = wrapIntoInterface frame filled
  where
    color' = getColor Nothing obj
    frame = makeFrame color' dim'
    filled_dim@(width', height') = mulVecX (quantitativeCurrent obj / quantitativeMax obj) dim'
    filled = alignNoScaleX Left frame (InterfaceElement filled_dim (0, 0) Nothing (color color' $ rectangleSolid width' height') (0, 0) Nothing)

fillQuantitativeBar :: (Show obj, Quantitative obj, Colorable obj) => obj -> InterfaceElement -> FillThisFrame -> FillerState
fillQuantitativeBar obj frame _ = do
    _ <- putTextIntoInterfaceRelative' (quantitativeName obj) color'
    _ <- putTextIntoInterfaceRelative' (show obj) white
    bar_frame <- fittingFrameWidthBounded transparent ((height frame) / 3) (setPadding padding frame) oneElement 0
    fillX $ filledRectangle obj bar_frame
  where
    putTextIntoInterfaceRelative' str color'' = fillX $ putTextIntoInterfaceRelative str color'' padding frame
    color' = getColor Nothing obj

fillRoute :: World -> Route -> InterfaceElement -> FillThisFrame -> FillerState
fillRoute world@(World _ _ _ _ hazards' (Game _ planet _ _ _ _) _ _) route@(Route end fuel' time) frame _ = do
    let fillTexts (text1, color1) (text2, color2) _ _ _ = do
            _ <- fillNameFrame $ largeScale $ putTextIntoInterfaceRelative text1 color1 (0, 0) frame
            let text2Interface = putTextIntoInterfaceRelative text2 color2 pad frame
            spacing <- spacerHeight frame (getTotalHeight [text2Interface])
            _ <- fillNameFrame spacing
            fillNameFrame text2Interface
    let fillThisFrame = fillX . fillFrameY name_frame Nothing
    _ <-
        mapM
            (\(left', right') -> fillThisFrame $ fillTexts left' right')
            [ ((end, white), ((show colliding_hazards), getColor Nothing colliding_hazards))
            , ((show fuel', fuelColor), (show time, timeColor))
            ]
    noFilling
  where
    fillNameFrame = fillY . alignX Left name_frame
    pad = mulVecY 0.2 padding
    name_frame = frame{dimensions = mulVecX 0.5 $ mulVecY 0.8 $ dimensions frame, interfacePicture = blank}
    connection = planetRouteToConnection world planet route
    colliding_hazards = CollidingHazards $ fromIntegral $ length $ filter (collidesConnectionHazard connection) hazards'
fillRoute _ _ _ _ = error "Can't fill route when not in game stage"

fillStatusPanelFrame :: FrameFiller World
fillStatusPanelFrame world@(World fuel' defence' _ _ _ (Game (Mission _ _ _ time') planet _ _ _ _) _ _) frame (Just fillThisFrame) = do
    _ <- fillTitleText "STATUS"
    _ <- fillThisFrame delimiterLine
    bar_frame' <- fittingFrameHeightBounded transparent delimiterWidth frame status_elements (height frame * 0.6)
    let bar_frame = setPaddingZero bar_frame'
    _ <- mapM (\e -> fillThisFrame $ fillFrameX bar_frame e fillQuantitativeBar) status_elements
    _ <- fillAlignDelimiterText "Location:" white (0, 0)
    _ <- fillAlignDelimiterText (show planet) mainColor (mulVecY 0.5 pad)
    _ <- fillThisFrame delimiterLine
    _ <- fillGameState world
    _ <- fillThisFrame delimiterLine
    _ <- mapM (fillAlignDelimiter) help_texts
    noFilling
  where
    fillAlignDelimiter = fillY . alignNoScaleX Left delimiterLine
    fillAlignDelimiterText t c p = fillAlignDelimiter $ mediumScale $ putTextIntoInterfaceRelative t c p frame
    fillTitleText t = fillAlignDelimiterText t white pad
    frameForElements elements = fittingFrameHeightBounded transparent delimiterWidth frame elements reserved
    pad = mulVecX 0 padding
    delimiterLine@(InterfaceElement (delimiterWidth, _) _ _ _ _ _) = makeDelimiterLine 0.8 frame
    status_elements = [fuel', defence', time']
    help_texts_pad = mulVec 0.5 padding
    help_texts = map (\t -> tinyScale $ putTextIntoInterface t white help_texts_pad) [selectHelpText, menuHelpText, mouseHelpText]
    reserved = (getTotalHeight $ delimiterLine : help_texts)
    fillGameState (World _ _ _ _ _ (Game _ _ _ _ routes' Normal) _ _) = do
        _ <- fillTitleText "ROUTES"
        _ <- fillThisFrame delimiterLine
        route_frame <- frameForElements routes'
        mapM (fillThisFrame . fillSelectableX world route_frame fillRoute) routes'
    fillGameState (World _ _ _ _ _ (Game _ _ _ _ _ (PlanetTransition _ _ time _)) _ _) = do
        _ <- fillTitleText "TRAVELING"
        time_frame <- frameForElements oneElement
        mapM (fillThisFrame) [delimiterLine, fillFrameX time_frame time fillQuantitativeBar]
    fillGameState _ = error "Can't fill game state when not in game state"
fillStatusPanelFrame _ _ _ = error "Status pannel can only be filled in game stage"

-- =========================================================
-- =================== Game panel render ===================
-- =========================================================

-- ======================== Map ============================

wrapNamedGameElement :: (Named obj) => obj -> InterfaceElement -> InterfaceElement
wrapNamedGameElement element element_interface = filled{gamePosition = gamePosition element_interface}
  where
    nameInterface = tinyScale $ putTextIntoInterface (getName element) white (0, 0)
    filled = wrapElementAndText element_interface nameInterface $ makeFrameFiller $ do fillY nameInterface

planetConnectionToInterfaceElement :: GameStageDrawer PlanetConnection
planetConnectionToInterfaceElement world connection@(PlanetConnection planet1@(Planet _ pos1 _) planet2@(Planet _ pos2 _) _) = connectionInterface
  where
    pos' = middlePoint pos1 pos2
    color' = getColor (Just world) (PlanetConnection planet1 planet2 Nothing)
    (Line zero_pos1 zero_pos2) = getRealLine (Line pos1 pos2) (0, 0)
    line' = color color' $ line [zero_pos1, zero_pos2]

    -- for double connection both triangles will have base at the middle of the line
    triangle_pos = movePoint (connectionArrowHeight / 2) (0, 0) zero_pos1
    triangle' = triangle color' triangle_pos zero_pos1 connectionArrowHeight
    connectionInterface = InterfaceElement (0, 0) (0, 0) (Just pos') (pictures [line', triangle']) (0, 0) (toHoverType connection)

planetConnections :: World -> Planet -> [InterfaceElement]
planetConnections world planet1 = map (\route -> planetConnectionToInterfaceElement world $ planetRouteToConnection world planet1 route) $ planetRoutes planet1 world

planetToInterfaceElement :: GameStageDrawer Planet
planetToInterfaceElement world planet@(Planet _ pos' effect) = wrapNamedGameElement planet filled_planet_interface
  where
    color' = getColor (Just world) planet
    planet_interface = InterfaceElement (putInVec $ getHeight planet) (0, 0) (Just pos') (circleFromHeight color' planetRadius) (0, 0) (toHoverType planet)
    effect_interface = tinyScale $ putTextIntoInterface (show effect) color' (0, 0)
    filled_planet_interface = wrapElementAndText planet_interface effect_interface $ makeFrameFiller $ do alignY Bottom planet_interface [effect_interface]

hazardToInterfaceElement :: GameStageDrawer Hazard
hazardToInterfaceElement _ hazard@(Hazard hazard_type _ pos' r _ _) = wrapNamedGameElement hazard hazardInterface
  where
    drawCircle a f r' = color (withAlpha a color') $ f r'
    color' = getColor Nothing hazard_type
    circles = map (\(a, f, r') -> drawCircle a f r') [(1 :: Float, circle, r), (1, circle, r * 0.8), (0.3, circleSolid, r)]
    hazardInterface = InterfaceElement (putInVec $ r * 2) (0, 0) (Just pos') (pictures circles) padding (toHoverType hazard)

fillPlanetsFrame :: FrameFiller World
fillPlanetsFrame world frame _ = do
    _ <- fillMap $ concatMap (planetConnections world) $ planets world
    _ <- fillExtra world frame
    _ <- fillThis planetToInterfaceElement $ planets world
    _ <- fillThis hazardToInterfaceElement (hazards world)
    noFilling
  where
    fillMap l = mapM addWithoutFilling l
    fillThis f elements = fillMap $ map (f world) elements
    fillExtra (World _ _ _ _ _ (Game _ _ _ _ _ (PlanetTransition (Ship cur_pos dest_pos _ _) _ _ _)) _ _) _ = do
        let ship_interface = InterfaceElement (putInVec shipHeight) cur_pos Nothing (triangle shipColor cur_pos dest_pos shipHeight) (0, 0) Nothing
        addWithoutFilling ship_interface
    fillExtra _ _ = noFilling

-- ====================== Hower ============================

getHoveredElement :: [InterfaceElement] -> VecFloat -> Maybe HoverableType
getHoveredElement elements mouse_pos = msum $ map hover $ filter isHovered elements
  where
    isHovered (InterfaceElement _ real_pos _ _ _ (Just (HoverableType check _))) = check real_pos mouse_pos
    isHovered _ = False

fillHover :: InterfaceElement -> Maybe HoverableType -> FillerState
fillHover frame (Just (HoverableType _ hoverable_texts)) = alignY Top frame{interfacePadding = (0, 0)} [hower_frame_with_content]
  where
    text_pad@(x_pad, _) = mulVec 0.5 padding
    hower_frame = setPadding (putInVec x_pad) $ fillFrameWithColor mainColor $ makeFrame mainColor howeredInfoDimensions
    to_fill = map (\t -> tinyScale $ putTextIntoInterface t white text_pad) hoverable_texts
    hower_frame_with_content = fillFrameY hower_frame Nothing $ makeFrameFiller $ do alignY CenterY hower_frame to_fill
fillHover _ _ = noFilling

-- =========================================================
-- ================== End screen render ====================
-- =========================================================

fillEnding :: FrameFiller World
fillEnding (World _ _ _ _ _ (Ending ending_type _) _ _) frame _ = alignY CenterY frame [main_text, help]
  where
    putAndScale t c p = smallScale $ putTextIntoInterface t c p
    main_text = putAndScale (show ending_type) endTextColor (0, 0)
    help = putAndScale menuHelpText white padding
fillEnding _ _ _ = error "Can't fill end screen when game didn't end"

-- =========================================================
-- ================= Main render functions =================
-- =========================================================

drawGameStage :: World -> InterfaceElement -> Picture
drawGameStage world frame = interfacePicture $ fillFrameX screenInterface world filler
  where
    planets_frame = makeFrame mainColor planetsFrameDimensions
    planets_frame_content = fillFrameData (0, []) Nothing planets_frame world fillPlanetsFrame
    filler = makeFrameFiller $ do
        planets_frame_filled <- fillX planets_frame
        planets_frame_filled_content <- gameElementsToInterfaceSpace planets_frame planets_frame_filled planets_frame_content
        _ <- fillHover (setPaddingZero planets_frame_filled) $ getHoveredElement planets_frame_filled_content $ mousePos world
        status_panel_frame <- fittingFrameWidthBounded (mainColor, background) (height planets_frame) frame oneElement 0
        fillX $ wrapIntoInterface status_panel_frame $ fillFrameY status_panel_frame world fillStatusPanelFrame

-- explanation in GameRunner
renderIO :: World -> IO Picture
renderIO world = pure $ renderNonIO world

renderNonIO :: World -> Picture
renderNonIO world@(World _ _ _ _ _ (Selection _) _ _) = interfacePicture $ fillFrameY missionSelectionFrame world fillMissionSelectionFrame
  where
    missionSelectionFrame = alignX CenterX screenInterface $ makeFrame mainColor missionSelectionDimensions
renderNonIO world@(World _ _ _ _ _ (Game _ _ _ _ _ _) _ _) = drawGameStage world screenInterface
renderNonIO world@(World _ _ _ _ _ (Ending _ _) _ _) = interfacePicture $ fillFrameY screenInterface world fillEnding
