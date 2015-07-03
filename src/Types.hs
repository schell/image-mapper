{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types where

import Linear
import Graphics.UI.GLFW
import Graphics.Text.TrueType
import Gelatin.Core.Render
import GHC.Generics (Generic)
import Control.Concurrent.Async
import Control.Varying
import Control.Lens
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Fresh
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import Data.Typeable
import Data.Hashable
import Data.IntMap (IntMap)

newtype AttachedRenderers = Attached { attached :: IntMap Renderer }
newtype UsedThisFrame = UsedThisFrame { usedThisFrame :: IntMap Int }

data Rez = Rez { rezGeom      :: GeomRenderSource
               , rezBez       :: BezRenderSource
               , rezWindow    :: Window
               , rezFontCache :: Async FontCache
               } deriving (Typeable)

type MakesScene r = ( ReadsRez r
                    , ModifiesRenderers r
                    , DoesIO r
                    )

type ReadsRez r = Member (Reader Rez) r
type DoesIO r = SetMember Lift (Lift IO) r
type StatsRenderers r = ( Member (State UsedThisFrame) r )

type ModifiesRenderers r = ( Member (State AttachedRenderers) r)

newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Enum, Ord, Num)

data AABB = AABB { aabbCenter   :: V2 Float
                 , aabbHalfSize :: V2 Float
                 } deriving (Show, Eq, Typeable)
makeLensesFor [("aabbCenter", "aabbCenter_")
              ,("aabbHalfSize", "aabbHalfSize_")
              ] ''AABB

deriving instance Eq Transform

deriving instance Generic PointSize

instance Hashable Picture where
    hashWithSalt s (Pic _ p w h) =
        s `hashWithSalt` p `hashWithSalt` w `hashWithSalt` h

instance Hashable PointSize

instance Hashable Label where
    hashWithSalt s (Label _ st fn p c) =
        s `hashWithSalt` st `hashWithSalt` fn `hashWithSalt` p `hashWithSalt` c

instance Hashable Box where
    hashWithSalt s (Box _ sz c) = s `hashWithSalt` sz `hashWithSalt` c

instance Hashable Button where
    hashWithSalt s (Button _ b l) = s `hashWithSalt` b `hashWithSalt` l

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

data Picture = Pic { picTransform :: Transform
                   , picPath      :: String
                   , picWidth     :: Int
                   , picHeight    :: Int
                   } deriving (Show, Eq, Typeable, Generic)

type Color = V4 Float
type Size = V2 Float
type Position = V2 Float
type Scale = V2 Float
type Rotation = Float

newtype Delta = Delta { unDelta :: Double }

instance Monoid InputEvent where
    mempty = NoInputEvent
    mappend NoInputEvent e = e
    mappend e _ = e

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
