{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types.Internal where

import Linear
import Graphics.UI.GLFW
import Graphics.Text.TrueType
import Gelatin.Core.Rendering
import GHC.Generics (Generic)
import Control.Concurrent.Async
import Control.Varying
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Fresh
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import Data.Typeable
import Data.Hashable
import Data.IntMap (IntMap)

newtype Requests = Requests { requests :: IntMap (Async RequestResult) }
newtype AttachedRenderings = Attached { attached :: IntMap Rendering }
newtype NamedRenderings = Named { named :: IntMap String }

data Rez = Rez { rezGeom      :: GeomRenderSource
               , rezBez       :: BezRenderSource
               , rezMask      :: MaskRenderSource
               , rezWindow    :: Window
               , rezFontCache :: Async FontCache
               } deriving (Typeable)

type MakesScene r = ( ReadsRez r
                    , ModifiesRenderings r
                    , DoesIO r
                    )

type ReadsRez r = Member (Reader Rez) r
type ModifiesTime r = Member (State Delta) r
type DoesIO r = SetMember Lift (Lift IO) r

type ModifiesRenderings r = ( Member (State AttachedRenderings) r
                            , Member (State NamedRenderings) r
                            )

instance Hashable Clip

data Clip = Clip { clipTopLeft     :: V2 Int
                 , clipBottomRight :: V2 Int
                 } deriving (Show, Eq, Typeable, Generic)

data AABB = AABB { aabbCenter   :: V2 Float
                 , aabbHalfSize :: V2 Float
                 } deriving (Show, Eq, Typeable)
--makeLensesFor [("aabbCenter", "aabbCenter_")
--              ,("aabbHalfSize", "aabbHalfSize_")
--              ] ''AABB

instance Hashable Transform
deriving instance Generic Transform
deriving instance Eq Transform

deriving instance Generic PointSize

type Color = V4 Float
type Size = V2 Float
type Position = V2 Float
type Vector = V2 Float
type Scale = V2 Float
type Rotation = Float

newtype Delta = Delta { unDelta :: Double } deriving (Show)

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
                | RequestEvent Uid RequestResult
                deriving (Show, Eq, Ord)

data Request = ReqPicInfo Uid FilePath deriving (Show, Eq, Ord)

data RequestResult = ResultPicInfo PicInfo deriving (Show, Eq, Ord)

data PicInfo = PicInfo { picInfoWidth  :: Int
                       , picInfoHeight :: Int
                       } deriving (Show, Eq, Ord)

newtype Uid = Uid { unUid :: Int } deriving (Show, Eq, Enum, Ord, Num)

type MakesUid r = Member (Fresh Uid) r

type TimeDelta r = Member (State Delta) r

type Vareff r = Var (Eff r)
