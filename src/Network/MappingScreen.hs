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
                              <*> pure []
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
pictureStr = (("Pic: " ++) . str) <$> currentPicture
    where str (Just p) = picPath p
          str _ = "Nothing"
--------------------------------------------------------------------------------
-- The picture the user is mapping
--------------------------------------------------------------------------------
mask :: (DoesIO r, ReadsRez r) => Vareff r InputEvent (Maybe Mask)
mask = mask' <$> pure mempty <*> currentPicture <*> mappingBG
    where mask' t (Just pic) bg = Just $ Mask t (Element pic) (Element bg)
          mask' _ _ _ = Nothing

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
instance Renderable MappingScreen where
    nameOf _ = "MappingScreen"
    render m@(MappingScreen _ b p (MSInfo ia ib) _) = do
        Renderer fb  _ <- getRenderer b
        Renderer fp  _ <- getRenderer p
        Renderer fia _ <- getRenderer ia
        Renderer fib _ <- getRenderer ib
        let f t = do fb  $ t <> transformOf b
                     fp  $ t <> transformOf p
                     fia $ t <> transformOf ia
                     fib $ t <> transformOf ib
            c = putStrLn $ "Cleaning a mapping screen (non-op) " ++ (show $ hash m)
        return $ Just $ Renderer f c

    transformOf = mappingScreenTfrm

    renderingHashes m@(MappingScreen _ b p (MSInfo ia ib) _) =
        hash m : concatMap renderingHashes [Element b, Element p, Element ia, Element ib]

instance Hashable MappingScreen where
    hashWithSalt s (MappingScreen _ bg p inf _) =
        s `hashWithSalt` bg `hashWithSalt` p `hashWithSalt` inf

instance Hashable MSInfo where
    hashWithSalt s (MSInfo a b) = s `hashWithSalt` a `hashWithSalt` b

data MappingScreen = MappingScreen { mappingScreenTfrm     :: Transform
                                   , mappingScreenBG       :: Box
                                   , mappingScreenPic      :: Maybe Mask
                                   , mappingScreenInfo     :: MSInfo
                                   , mappingScreenRequests :: [Request]
                                   } deriving (Show, Eq, Typeable)

data MSInfo = MSInfo { msInfoPic  :: Label
                     , msInfoZoom :: Label
                     } deriving (Show, Eq, Typeable)
