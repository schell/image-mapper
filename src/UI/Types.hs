{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

data UITransform = UITransform { uiPosition  :: V2 Float
                               , uiSize      :: V2 Float
                               , uiScale     :: V2 Float
                               , uiRotation  :: Float
                               } deriving (Show, Eq, Typeable)
makeLensesFor [("uiUid", "uiUid_")
              , ("uiPosition", "uiPosition_")
              , ("uiSize",   "uiSize_")
              , ("uiScale",  "uiScale_")
              , ("uiRotation", "uiRotation_")
              ] ''UITransform

instance Monoid UITransform where
    mempty = UITransform 0 0 1 0
    mappend (UITransform p sz sc r) (UITransform p' sz' sc' r') =
        UITransform (p + p') (max sz sz') (sc * sc') (r + r')

data UIElement = UILabel { uiLabelString          :: String
                         , uiLabelTextColor       :: V4 Float
                         , uiLabelBackgroundColor :: V4 Float
                         }
               deriving (Show, Eq, Typeable)

data UITree a = UILeaf Uid UITransform a
              | UIBranch Uid UITransform [UITree a]
              deriving (Show)

type UserInterface = UITree UIElement
type Scene = UITree Renderer

--getTransform :: UIElement -> Transform
--getUid :: UIElement -> Uid
