{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Network.MappingScreen (
    mappingScreen,
    MappingScreen(..),
    MSInfo(..),
    mappingScreenMode,
    MappingScreenMode(..)
) where

import Prelude hiding (until)
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
import UI.Box
import UI.Mask
import System
import Network.System
import Data.Hashable
import Data.Monoid
import Data.Typeable

mappingScreen :: (DoesIO r, ReadsRez r) => Vareff r InputEvent MappingScreen
mappingScreen = MappingScreen <$> pure mempty
                              <*> mappingBG
                              <*> mask
                              <*> info

mappingScreenMode :: Monad m => Var m a MappingScreenMode
mappingScreenMode = pure MappingScreenModeNavigation
--------------------------------------------------------------------------------
-- Displayed info about the mapping screen
--------------------------------------------------------------------------------
info :: (DoesIO r, ReadsRez r) => Vareff r InputEvent MSInfo
info = MSInfo <$> infoLabelPic <*> infoLabelZoom

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

zoomPos :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Position
zoomPos = mappingLowerLeft + (pure $ V2 0 12)

zoomStr :: Monad m => Var m InputEvent String
zoomStr = (("Zoom: " ++) . show) <$> scrollScale

pictureStr :: DoesIO r => Vareff r InputEvent String
pictureStr = (("Pic: " ++) . str) <$> droppedPicture
    where str (Just p) = picPath p
          str _ = "Nothing"
--------------------------------------------------------------------------------
-- The picture the user is mapping
--------------------------------------------------------------------------------
mask :: (DoesIO r, ReadsRez r) => Vareff r InputEvent (Maybe Mask)
mask = mask' <$> pure mempty <*> currentPicture <*> mappingBG
    where mask' t (Just pic) bg = Just $ Mask t (Element pic) (Element bg)
          mask' _ _ _ = Nothing

currentPicture :: (DoesIO r, ReadsRez r) => Vareff r InputEvent (Maybe Picture)
currentPicture = tfrm <$> (Transform <$> currentPicturePosition
                                     <*> scrollScale <*> 0)
                      <*> droppedPicture
    where tfrm t' (Just (Pic t p w h)) = Just $ Pic (t' <> t) p w h
          tfrm _ Nothing = Nothing

currentPicturePosition :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Position
currentPicturePosition = mappingUpperLeft + pictureOffset + pictureDragOffset

pictureDragOffset :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Position
pictureDragOffset =
    dragging `until` fileDropped `andThenE` (once 0) `andThen` pictureDragOffset
        where dragging = accumulate (+) 0 <~ startingWith 0 <~ mouseButtonDragged MouseButton'1

pictureOffset :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Position
pictureOffset = mappingCenter - ((check <$> pictureSizePerceived) / 2)
    where check (Just v) = v
          check _ = 0

pictureSizePerceived :: DoesIO r => Vareff r InputEvent (Maybe Size)
pictureSizePerceived = f <$> scrollScale <*> droppedPicture
    where f sc (Just p) = Just $ sc * (pictureSize p)
          f _ _ = Nothing

scrollScale :: Monad m => Var m InputEvent Scale
scrollScale =
    scaling `until` fileDropped `andThenE` (once 1) `andThen` scrollScale
        where scaling = scroll ~> accumulate accf 1
              scroll = 0 `orE` mouseScrolled
              accf acc (V2 _ y) = max 0.1 (acc + V2 y y)

droppedPicture :: DoesIO r => Vareff r InputEvent (Maybe Picture)
droppedPicture = startingWith Nothing <~ tagM (lift . loadPicture) fileDropped
--------------------------------------------------------------------------------
-- The mapping area
--------------------------------------------------------------------------------
mappingBG :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Box
mappingBG = Box <$> (Transform <$> mappingUpperLeft <*> 1 <*> 0)
                <*> mappingSize <*> pure grey

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
-- Types
--------------------------------------------------------------------------------
instance Renderable MappingScreen where
    cache = cacheChildren
    nameOf _ = "MappingScreen"
    transformOf = mappingScreenTfrm
    children (MappingScreen _ b p (MSInfo ia ib)) =
        [Element b, Element p, Element ia, Element ib]
    hashes m@(MappingScreen _ b p (MSInfo ia ib)) =
        hash m : concat [hashes b, hashes p, hashes ia, hashes ib]

instance Hashable MappingScreen where
    hashWithSalt s (MappingScreen _ bg p inf) =
        s `hashWithSalt` bg `hashWithSalt` p `hashWithSalt` inf

instance Hashable MSInfo where
    hashWithSalt s (MSInfo a b) = s `hashWithSalt` a `hashWithSalt` b

data MappingScreen = MappingScreen { mappingScreenTfrm     :: Transform
                                   , mappingScreenBG       :: Box
                                   , mappingScreenPic      :: Maybe Mask
                                   , mappingScreenInfo     :: MSInfo
                                   } deriving (Show, Eq, Typeable)

instance Show MappingScreenMode where
    show MappingScreenModeNavigation = "Navigation"

data MappingScreenMode = MappingScreenModeNavigation deriving (Eq)

data MSInfo = MSInfo { msInfoPic  :: Label
                     , msInfoZoom :: Label
                     } deriving (Show, Eq, Typeable)
