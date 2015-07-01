{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Network.System where

import UI.Types
import Scene
import Prelude hiding (sequence_)
import Linear
import Graphics.UI.GLFW hiding (Image(..))
import Gelatin.Core.Triangulation.Common
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Control.Varying
import Control.Varying.Time
import Control.Eff.State.Strict
import Control.Arrow

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------
windowResized :: Monad m => Var m InputEvent (Event (Int, Int))
windowResized = arr check ~> onJust
    where check (WindowSizeEvent w h) = Just (w,h)
          check _ = Nothing

cursorMoved :: Monad m => Var m InputEvent (Event (Double, Double))
cursorMoved = arr check ~> onJust
    where check (CursorMoveEvent x y) = Just (x,y)
          check _ = Nothing

mouseAction :: Monad m => Var m InputEvent (Event (MouseButton, MouseButtonState, ModifierKeys))
mouseAction = arr check ~> onJust
    where check (MouseButtonEvent mb mbs mks) = Just (mb,mbs,mks)
          check _ = Nothing

mouseDown :: Monad m => Var m InputEvent (Event (MouseButton, ModifierKeys))
mouseDown = mouseAction ~> arr (check . toMaybe) ~> onJust
    where check (Just (mb, MouseButtonState'Pressed, mks)) = Just (mb, mks)
          check _ = Nothing

mouseUp :: Monad m => Var m InputEvent (Event (MouseButton, ModifierKeys))
mouseUp = mouseAction ~> arr (check . toMaybe) ~> onJust
    where check (Just (mb, MouseButtonState'Released, mks)) = Just (mb, mks)
          check _ = Nothing

mb1Press :: Monad m => Var m InputEvent (Event (Double, Double))
mb1Press = latchWith const cursorMoved mouseDown

cursorInside :: Monad m => Var m InputEvent Poly -> Var m InputEvent (Event ())
cursorInside vpoly = proc e -> do
    poly    <- vpoly -< e
    (mx,my) <- cursorStartingAt (0, 0) -< e
    let [mx', my'] = map realToFrac [mx,my]
    onTrue -< pointInside (V2 mx' my') poly

filesDropped :: Monad m => Var m InputEvent (Event [String])
filesDropped = arr check ~> onJust
    where check (FileDropEvent path) = Just path
          check _ = Nothing

fileDropped :: Monad m => Var m InputEvent (Event String)
fileDropped = (head <$>) <$> filesDropped
--------------------------------------------------------------------------------
-- Continuous Streams
--------------------------------------------------------------------------------
cursorStartingAt :: Monad m => (Double, Double) -> Var m InputEvent (Double, Double)
cursorStartingAt = (cursorMoved ~>) . startingWith

windowSize :: (Member (Reader Rez) r, SetMember Lift (Lift IO) r)
           => Vareff r InputEvent (Int, Int)
windowSize = Var $ \_ -> do
    win <- rezWindow <$> ask
    (w,h) <- lift $ getWindowSize win
    return ((w,h), windowResized ~> startingWith (w,h))

time :: (TimeDelta r, RealFrac a) => Vareff r b a
time = realToFrac <$> delta (unDelta <$> get) (-)
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
(#) :: a -> (a -> b) -> b
(#) = flip ($)
infixl 0 #
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data ButtonState = ButtonStateOff | ButtonStateHover | ButtonStateOn
data ButtonGraph m = ButtonGraph { buttonGraphBtn   :: Var m InputEvent Button
                                 , buttonGraphPoly  :: Var m InputEvent Poly
                                 , buttonGraphState :: Var m InputEvent ButtonState
                                 }
