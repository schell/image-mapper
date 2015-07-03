{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Network.MappingScreen (mappingScreen, MappingScreen(..), MSInfo(..)) where

import Prelude hiding (until)
import Control.Varying
import Control.Arrow
import Control.Eff.Lift
import Gelatin.Core.Color
import Gelatin.Core.Render
import Graphics.Text.TrueType
import Linear hiding (ex)
import Types
import System
import Network.System
import Data.Hashable
import Data.Monoid
import Data.Typeable

mappingScreen :: (DoesIO r, ReadsRez r) => Vareff r InputEvent MappingScreen
mappingScreen = MappingScreen <$> pure mempty <*> mappingBG <*> currentPicture <*> info
--------------------------------------------------------------------------------
-- Displayed info about the mapping screen
--------------------------------------------------------------------------------
info :: (DoesIO r, ReadsRez r) => Vareff r InputEvent MSInfo
info = MSInfo <$> infoLabelPic <*> infoLabelZoom

infoLabelZoom :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Label
infoLabelZoom =
    Label <$> (Transform <$> mappingLowerLeft <*> 1 <*> 0)
          <*> zoomStr <*> pure "Arial" <*> pure (PointSize 12) <*> pure white

infoLabelPic :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Label
infoLabelPic =
    Label <$> (Transform <$> (mappingLowerLeft + pure (V2 0 14))
                         <*> 1 <*> 0)
          <*> pictureStr <*> pure "Arial" <*> pure (PointSize 12) <*> pure white

zoomStr :: Monad m => Var m InputEvent String
zoomStr = (("Zoom: " ++) . show) <$> scrollScale

pictureStr :: DoesIO r => Vareff r InputEvent String
pictureStr = (("Pic: " ++) . str) <$> currentPicture
    where str (Just p) = picPath p
          str _ = "Nothing"
--------------------------------------------------------------------------------
-- The picture the user is mapping
--------------------------------------------------------------------------------
currentPicture :: DoesIO r => Vareff r InputEvent (Maybe Picture)
currentPicture = tfrm <$> (Transform <$> mappingUpperLeft <*> scrollScale <*> 0)
                      <*> droppedPicture
    where tfrm t' (Just (Pic t p w h)) = Just $ Pic (t' <> t) p w h
          tfrm _ Nothing = Nothing

scrollScale :: Monad m => Var m InputEvent Scale
scrollScale =
    (scaling `until` fileDropped `andThenE` (once (V2 1 1)) `andThen` scrollScale)
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
instance Hashable MappingScreen where
    hashWithSalt s (MappingScreen _ bg p inf) =
        s `hashWithSalt` bg `hashWithSalt` p `hashWithSalt` inf

instance Hashable MSInfo where
    hashWithSalt s (MSInfo a b) = s `hashWithSalt` a `hashWithSalt` b

data MappingScreen = MappingScreen { mappingScreenTfrm :: Transform
                                   , mappingScreenBG   :: Box
                                   , mappingScreenPic  :: Maybe Picture
                                   , mappingScreenInfo :: MSInfo
                                   } deriving (Show, Eq, Typeable)

data MSInfo = MSInfo { msInfoPic  :: Label
                     , msInfoZoom :: Label
                     } deriving (Show, Eq, Typeable)
