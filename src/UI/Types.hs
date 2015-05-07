{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
module UI.Types where

import Gelatin.Core.Render
import Data.Typeable
import Data.Generic.Diff

newtype Uid = Uid Int deriving (Show, Eq, Enum, Ord)

data AABB = AABB { aabbCenter   :: V2 Float
                 , aabbHalfSize :: V2 Float
                 } deriving (Show, Eq, Typeable)

data UI = UI { uiUid       :: Uid
             , uiPosition  :: V2 Float
             , uiSize      :: V2 Float
             , uiScale     :: V2 Float
             , uiRotation  :: Float
             } deriving (Show, Eq, Typeable)

data UIElement = UIContainer { uiContainer         :: UI
                             , uiContainerElements :: [UIElement]
                             }
               -- | UILabel { uiLabel                :: UI
               --          , uiLabelString          :: String
               --          , uiLabelTextColor       :: V4 Float
               --          , uiLabelBackgroundColor :: V4 Float
               --          }
               deriving (Show, Eq, Typeable)

getTransform :: UIElement -> Transform
getTransform UIContainer{uiContainer=UI{..}} =
    Transform uiPosition uiScale uiRotation
--getTransform UILabel{uiLabel=UI{..}} =
--    Transform uiPosition uiScale uiRotation

getUid :: UIElement -> Uid
getUid UIContainer{uiContainer=UI{..}} = uiUid
--getUid UILabel{uiLabel=UI{..}} = uiUid
