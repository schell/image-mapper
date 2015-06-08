{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UI.Types where

import Gelatin.Core.Render
import GHC.Generics (Generic)
import Data.Typeable
import Data.Hashable
import Control.Varying
import Control.Lens
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.State.Strict

newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Enum, Ord, Num)

data AABB = AABB { aabbCenter   :: V2 Float
                 , aabbHalfSize :: V2 Float
                 } deriving (Show, Eq, Typeable)
makeLensesFor [("aabbCenter", "aabbCenter_")
              ,("aabbHalfSize", "aabbHalfSize_")
              ] ''AABB

type Color = V4 Float
type Size = V2 Float
type Position = V2 Float
type Scale = V2 Float
type Rotation = Float

data Label = Label { labelTransform      :: Transform
                   , labelString         :: String
                   , labelFontFamilyName :: String
                   , labelFontPointSize  :: PointSize
                   , labelColor          :: Color
                   } deriving (Show, Eq, Typeable, Generic)

data Box = Box { boxTransform :: Transform
               , boxSize      :: Size
               , boxColor     :: Color
               } deriving (Show, Eq, Typeable, Generic)

data Button = Button { buttonTransform :: Transform
                     , buttonBox       :: Box
                     , buttonLabel     :: Label
                     } deriving (Show, Eq, Typeable, Generic)

deriving instance Eq Transform
deriving instance Generic Transform
deriving instance Generic PointSize
instance Hashable Transform
instance Hashable PointSize
instance Hashable Label
instance Hashable Box
instance Hashable Button

newtype Delta = Delta { unDelta :: Double }

data InputEvent = NoInputEvent
                | CharEvent Char
                | WindowSizeEvent Int Int
                | KeyEvent Key Int KeyState ModifierKeys
                -- ^ Key, scancode, pressed/released, mods
                | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
                | CursorMoveEvent Double Double
                | CursorEnterEvent CursorState
                | ScrollEvent Double Double
                | FileDropEvent [String]
                deriving (Show, Eq, Ord)

type MakesUid r = Member (Fresh Uid) r

type TimeDelta r = Member (State Delta) r

type Vareff r = Var (Eff r)
