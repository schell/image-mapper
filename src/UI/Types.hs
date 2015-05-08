{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module UI.Types where

import Gelatin.Core.Render
import Data.Typeable
import Control.Lens

newtype Uid = Uid Int deriving (Show, Eq, Enum, Ord, Num)

data AABB = AABB { aabbCenter   :: V2 Float
                 , aabbHalfSize :: V2 Float
                 } deriving (Show, Eq, Typeable)
makeLensesFor [("aabbCenter", "aabbCenter_")
              ,("aabbHalfSize", "aabbHalfSize_")
              ] ''AABB

data UI = UI { uiUid       :: Uid
             , uiPosition  :: V2 Float
             , uiSize      :: V2 Float
             , uiScale     :: V2 Float
             , uiRotation  :: Float
             } deriving (Show, Eq, Typeable)
makeLensesFor [("uiUid", "uiUid_")
              , ("uiPosition", "uiPosition_")
              , ("uiSize",   "uiSize_")
              , ("uiScale",  "uiScale_")
              , ("uiRotation", "uiRotation_")
              ] ''UI

data UIElement = UIContainer { uiContainer         :: UI
                             , uiContainerElements :: [UIElement]
                             }
               | UILabel { uiLabel                :: UI
                         , uiLabelString          :: String
                         , uiLabelTextColor       :: V4 Float
                         , uiLabelBackgroundColor :: V4 Float
                         }
               deriving (Show, Eq, Typeable)

mkUi :: Uid -> UI
mkUi uid = UI uid zero zero (V2 1 1) 0

getUi :: UIElement -> UI
getUi (UIContainer u _) = u
getUi (UILabel u _ _ _) = u

--getTransform :: UIElement -> Transform
--getUid :: UIElement -> Uid
