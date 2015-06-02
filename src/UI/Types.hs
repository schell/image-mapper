{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UI.Types where

import Gelatin.Core.Render
import Data.Typeable
import Control.Lens

newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Enum, Ord, Num)

data AABB = AABB { aabbCenter   :: V2 Float
                 , aabbHalfSize :: V2 Float
                 } deriving (Show, Eq, Typeable)
makeLensesFor [("aabbCenter", "aabbCenter_")
              ,("aabbHalfSize", "aabbHalfSize_")
              ] ''AABB

data UIElement = UILabel { uiLabelString          :: String
                         , uiLabelFontFamilyName  :: String
                         , uiLabelFontPointSize   :: PointSize
                         , uiLabelFontColor       :: V4 Float
                         , uiLabelBackgroundColor :: V4 Float
                         }
               | UIBox { uiBoxAABB  :: AABB
                       , uiBoxColor :: V4 Float
                       }
               deriving (Show, Eq, Typeable)

data UITree a = UILeaf Uid Transform a
              | UIBranch Uid Transform [UITree a]
              deriving (Show)
