{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Network.MappingScreen (
    cursorMode,
    mappingScreen,
    MappingScreen(..),
    mappingScreenMode,
    MappingScreenMode(..)
) where

import Prelude hiding (until)
import Control.Monad
import Control.Varying
import Control.Arrow
import Control.Eff.Lift
import Gelatin.Core.Color
import Gelatin.Core.Rendering
import Graphics.Text.TrueType
import Graphics.UI.GLFW
import Linear hiding (ex)
import Types.Internal
import Types.Renderable
import UI.Label
import UI.Picture
import UI.Pattern
import UI.Box
import UI.Mask
import UI.HitArea
import System
import Network.System
import Data.Hashable
import Data.Monoid
import Data.Typeable
import Data.List
--------------------------------------------------------------------------------
-- The cursor signal for this screen.
--------------------------------------------------------------------------------
cursorMode :: Monad m => Var m InputEvent StandardCursorShape
cursorMode = mappingScreenMode ~> var mapCursor
    where mapCursor MappingScreenModeNavigation = StandardCursorShape'HResize
          mapCursor MappingScreenModeHitArea = StandardCursorShape'Crosshair
          mapCursor _ = StandardCursorShape'Arrow
--------------------------------------------------------------------------------
-- The mapping screen itself.
--------------------------------------------------------------------------------
mappingScreen :: (DoesIO r, ReadsRez r) => Vareff r InputEvent MappingScreen
mappingScreen = MappingScreen <$> mappingPattern
                              <*> mask
                              <*> info
--------------------------------------------------------------------------------
-- Displayed info about the mapping screen
--------------------------------------------------------------------------------
info :: (DoesIO r, ReadsRez r) => Vareff r InputEvent [Label]
info = sequenceA [infoLabelPic, infoLabelZoom, infoLabelTrans]

infoLabelTrans :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Label
infoLabelTrans =
    Label <$> (Transform <$> transPos <*> 1 <*> 0)
          <*> transStr
          <*> systemPathForFont "Arial" False False
          <*> pure (PointSize 12)
          <*> pure white

infoLabelZoom :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Label
infoLabelZoom =
    Label <$> (Transform <$> zoomPos <*> 1 <*> 0)
          <*> zoomStr
          <*> systemPathForFont "Arial" False False
          <*> pure (PointSize 12)
          <*> pure white

infoLabelPic :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Label
infoLabelPic =
    Label <$> (Transform <$> (zoomPos + pure (V2 0 14))
                         <*> 1 <*> 0)
          <*> pictureStr
          <*> systemPathForFont "Arial" False False
          <*> pure (PointSize 12)
          <*> pure white

transPos :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Position
transPos = mappingLowerLeft + (pure $ V2 0 12)

transStr :: (DoesIO r, ReadsRez r) => Vareff r InputEvent String
transStr = (++) <$> offset <*> absolute
    where absolute,offset :: (DoesIO r, ReadsRez r)
                          => Vareff r InputEvent String
          absolute = str <$> navigationModePicturePosition
          str :: V2 Float -> String
          str = (\s -> " (" ++ s ++ ")") . stringify
          offset = stringify <$> pictureDragOffset
          stringify (V2 x y) = concat ["x: ", show x, ", y: ", show y]

zoomPos :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Position
zoomPos = transPos + (pure $ V2 0 14)

zoomStr :: Monad m => Var m InputEvent String
zoomStr = (("Zoom: " ++) . show) <$> ((\(V2 x _) -> x) <$> pictureScale)

pictureStr :: DoesIO r => Vareff r InputEvent String
pictureStr = (("Pic: " ++) . str) <$> droppedPicture
    where str (Just p) = picPath p
          str _ = "Nothing"
--------------------------------------------------------------------------------
-- The mask that includes the current picture and hitareas
--------------------------------------------------------------------------------
mask :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Mask
mask = Mask <$> pure mempty <*> (Element <$> els)
            <*> (Element <$> (Box <$> (Transform <$> mappingUpperLeft <*> 1 <*> 0)
                                  <*> mappingSize
                                  <*> pure white))
    where els :: (DoesIO r, ReadsRez r) => Vareff r InputEvent [Element]
          els = sequenceA [ Element <$> currentPicture
                          , Element <$> hitAreas
                          , Element <$> editingHitArea
                          ]
--------------------------------------------------------------------------------
-- The picture the user is mapping
--------------------------------------------------------------------------------
currentPicture :: (DoesIO r, ReadsRez r) => Vareff r InputEvent (Maybe Picture)
currentPicture = tfrm <$> (Transform <$> navigationModePicturePosition
                                     <*> pictureScale <*> 0)
                      <*> droppedPicture
    where tfrm t' (Just (Pic t p w h)) = Just $ Pic (t' <> t) p w h
          tfrm _ Nothing = Nothing

navigationModePicturePosition :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Position
navigationModePicturePosition = mappingUpperLeft + pictureOffset + pictureDragOffset

pictureDragOffset :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Position
pictureDragOffset =
    dragging `until` fileDropped `andThenE` (once 0) `andThen` pictureDragOffset
        where dragging = accumulate (+) 0 <~ startingWith 0 <~ navDragging
              navDragging = combineWith (flip const) inNavigationMode
                                                     (mouseButtonDragged MouseButton'1)

pictureOffset :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Position
pictureOffset = mappingCenter - ((check <$> pictureSizePerceived) / 2)
    where check (Just v) = v
          check _ = 0

pictureSizePerceived :: DoesIO r => Vareff r InputEvent (Maybe Size)
pictureSizePerceived = f <$> pictureScale <*> droppedPicture
    where f sc (Just p) = Just $ sc * (pictureSize p)
          f _ _ = Nothing

pictureScale :: Monad m => Var m InputEvent Scale
pictureScale =
    scaling `until` fileDropped `andThenE` (once 1) `andThen` pictureScale
        where scaling = scroll ~> accumulate accf 1
              scroll = 0 `orE` mouseScrolled
              accf acc (V2 _ y) = max 0.1 (acc + V2 y y)

droppedPicture :: DoesIO r => Vareff r InputEvent (Maybe Picture)
droppedPicture = startingWith Nothing <~ tagM (lift . loadPicture) fileDropped
--------------------------------------------------------------------------------
-- The drawn hit areas
--------------------------------------------------------------------------------
editingHitArea :: (ReadsRez r, DoesIO r) => Vareff r InputEvent HitArea
editingHitArea = HitArea <$> (Transform <$> 0
                                        <*> pictureScale <*> 0)
                         <*> (collect <~ newPoint)
                         <*> pure (red `alpha` 0.8)
                         <*> pure True

newPoint :: Monad m => Var m InputEvent (Event Position)
newPoint = mouseButtonPressed MouseButton'1

hitAreas :: Monad m => Var m InputEvent [HitArea]
hitAreas = collect <~ newHitArea

newHitArea :: Monad m => Var m InputEvent (Event HitArea)
newHitArea = never
--------------------------------------------------------------------------------
-- The mapping area
--------------------------------------------------------------------------------
mappingPattern :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Pattern
mappingPattern =
    Pattern <$> (Transform <$> mappingUpperLeft <*> 1 <*> 0)
            <*> pure (Element [box1,box2,box3,box4])
            <*> (pure $ Clip 0 $ 2 * (floor <$> V2 sz sz))
            <*> pure 0
            <*> mappingSize
    where sz = 5
          box1 = grayBox
          box2 = whiteBox{ boxTransform = Transform (V2 sz 0) 1 0}
          box3 = whiteBox{ boxTransform = Transform (V2 0 sz) 1 0}
          box4 = grayBox { boxTransform = Transform (V2 sz sz) 1 0}
          grayBox = Box mempty (V2 sz sz) $ V4 0.7 0.7 0.7 1
          whiteBox = Box mempty (V2 sz sz) $ V4 1 1 1 1


mappingSize :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Size
mappingSize = mappingLowerRight - mappingUpperLeft

mappingCenter :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Position
mappingCenter = mappingUpperLeft + (mappingSize / 2)

mappingUpperLeft :: Monad m => Var m a Position
mappingUpperLeft = pure $ V2 10 10

mappingLowerLeft :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Position
mappingLowerLeft = proc e -> do
    V2 _ y <- mappingLowerRight -< e
    V2 x _ <- mappingUpperLeft -< e
    returnA -< V2 x y

mappingLowerRight :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Position
mappingLowerRight = windowSize - (pure $ V2 10 100)
--------------------------------------------------------------------------------
-- Modes
--------------------------------------------------------------------------------
inNavigationMode :: Monad m => Var m InputEvent (Event ())
inNavigationMode = use () mode
    where mode = mappingScreenMode ~> onWhen (== MappingScreenModeNavigation)

mappingScreenMode :: Monad m => Var m InputEvent MappingScreenMode
mappingScreenMode = toEnum <$> triggeredMode
    where triggeredMode = triggers ~> triggeredIndex ~> startingWith 0

triggers :: Monad m => Var m InputEvent [Event ()]
triggers = sequenceA [triggerDefault, triggerNav, triggerHitAreas, triggerProducts]

triggeredIndex :: (Monad m) => Var m [Event ()] (Event Int)
triggeredIndex = (var (elemIndex $ Event ())) ~> onJust

triggerDefault :: Monad m => Var m InputEvent (Event ())
triggerDefault = use () fileDropped

triggerNav :: Monad m => Var m InputEvent (Event ())
triggerNav = var (== CharEvent 'n') ~> onTrue

triggerHitAreas :: Monad m => Var m InputEvent (Event ())
triggerHitAreas = var (== CharEvent 'h') ~> onTrue

triggerProducts :: Monad m => Var m InputEvent (Event ())
triggerProducts = var (== CharEvent 'p') ~> onTrue
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
instance Renderable MappingScreen where
    cache rz rs (MappingScreen bg p i) = foldM (cacheIfNeeded rz) rs
                                                 [ Element bg
                                                 , Element p
                                                 , Element i
                                                 ]
    nameOf _ = "MappingScreen"
    renderLayerOf (MappingScreen bg p i) = layer
        where layer = concatMap renderLayerOf [ Element bg
                                              , Element p
                                              , Element i
                                              ]

instance Hashable MappingScreen where
    hashWithSalt s (MappingScreen bg p inf) =
        s `hashWithSalt` bg `hashWithSalt` p `hashWithSalt` inf

data MappingScreen = MappingScreen { mappingScreenPattern  :: Pattern
                                   , mappingScreenPic      :: Mask
                                   , mappingScreenInfo     :: [Label]
                                   } deriving (Show, Eq, Typeable)

instance Show MappingScreenMode where
    show MappingScreenModeDefault = "Default"
    show MappingScreenModeNavigation = "Navigation"
    show MappingScreenModeHitArea = "Hit Areas"
    show MappingScreenModeProducts = "Products"

data MappingScreenMode = MappingScreenModeDefault
                       | MappingScreenModeNavigation
                       | MappingScreenModeHitArea
                       | MappingScreenModeProducts
                       deriving (Eq, Enum)
