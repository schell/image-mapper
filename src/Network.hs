{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network where

import Scene
import UI
import Prelude hiding (sequence_)
import System.Remote.Monitoring
import Gelatin.Core.Render
import Control.Concurrent.Async
import Graphics.GL.Core33
import Control.Varying
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Fresh
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Arrow
import Data.Bits
import Data.Typeable
import Data.IORef
import Data.Monoid
--import Data.Maybe
--import Data.Foldable (sequence_)
import Data.IntMap (IntMap)
--import Data.Map (Map)
import Data.Time.Clock
import System.Exit
import qualified Data.IntMap as IM
--import qualified Data.Map as M

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

type DoesIO r = SetMember Lift (Lift IO) r

type MakesUid r = Member (Fresh Uid) r

type TimeDelta r = Member (State Delta) r

type Vareff r = Var (Eff r)

cursorMoved :: Monad m => Var m InputEvent (Maybe (Double, Double))
cursorMoved = arr check
    where check (CursorMoveEvent x y) = Just (x,y)
          check _ = Nothing

cursorPosition :: Monad m => Var m InputEvent (Event (Double, Double))
cursorPosition = cursorMoved ~> onJust

cursorStartingAt :: Monad m => (Double, Double) -> Var m InputEvent (Double, Double)
cursorStartingAt = (cursorPosition ~>) . startingWith

mouseAction :: Monad m => Var m InputEvent (Maybe (MouseButton, MouseButtonState, ModifierKeys))
mouseAction = arr check
    where check (MouseButtonEvent mb mbs mks) = Just (mb,mbs,mks)
          check _ = Nothing

mouseDown :: Monad m => Var m InputEvent (Event (MouseButton, ModifierKeys))
mouseDown = mouseAction ~> arr check ~> onJust
    where check (Just (mb, MouseButtonState'Pressed, mks)) = Just (mb, mks)
          check _ = Nothing

mouseUp :: Monad m => Var m InputEvent (Event (MouseButton, ModifierKeys))
mouseUp = mouseAction ~> arr check ~> onJust
    where check (Just (mb, MouseButtonState'Released, mks)) = Just (mb, mks)
          check _ = Nothing

mb1Press :: Monad m => Var m InputEvent (Event (Double, Double))
mb1Press = latchWith const cursorPosition mouseDown

time :: TimeDelta r => Vareff r b Double
time = delta (unDelta <$> get) (-)

freshUid :: MakesUid r => Var (Eff r) a Uid
freshUid = Var $ \_ -> do
    uid <- fresh
    return (uid, pure uid)

uinetwork :: (MakesUid r, TimeDelta r)
          => Var (Eff r) InputEvent [UITree UIElement]
uinetwork = (:[]) <$> tree
    where tree = UILeaf <$> freshUid <*> t <*> l
          ab = AABB (V2 0 0) (V2 50 50)
          l = UIBox <$> pure ab <*> ((fmap realToFrac) <$> c)
          t = proc e -> do
                  (x,y) <- mb1Press ~> startingWith (100, 100) -< e
                  returnA -< translate x y mempty
          c = time ~> V4 <$> r <*> g <*> b <*> 1
              where r = tween easeInExpo 0 1 1 `andThenE` tween easeInExpo 1 0 3 `andThen` r
                    g = tween easeInExpo 0 1 2 `andThenE` tween easeInExpo 1 0 4 `andThen` g
                    b = tween easeInExpo 0 1 3 `andThenE` tween easeInExpo 1 0 5 `andThen` b
