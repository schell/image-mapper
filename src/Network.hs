{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Network where

import UI.Types
import Scene
import Prelude hiding (sequence_)
import Gelatin.Core.Render hiding (scaled)
import Gelatin.Core.Triangulation.Common
import Control.Varying
import Control.Eff.State.Strict
import Control.Arrow

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
(#) :: a -> (a -> b) -> b
(#) = flip ($)
infixl 0 #
--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------
-- Basic streams
--------------------------------------------------------------------------------
cursorStartingAt :: Monad m => (Double, Double) -> Var m InputEvent (Double, Double)
cursorStartingAt = (cursorMoved ~>) . startingWith

time :: (TimeDelta r, RealFrac a) => Vareff r b a
time = realToFrac <$> delta (unDelta <$> get) (-)

translated :: Monad m
           => Var m a (V2 Float) -> Var m a (b, Transform) -> Var m a (b, Transform)
translated vVec vEl = proc a -> do
    l       <- vEl  -< a
    (V2 x y) <- vVec -< a
    returnA -< translate x y <$> l

scaled :: Monad m
       => Var m a (V2 Float) -> Var m a (b, Transform) -> Var m a (b, Transform)
scaled vVec vEl = proc a -> do
    l       <- vEl  -< a
    (V2 x y) <- vVec -< a
    returnA -< scale x y <$> l

rotated :: Monad m
       => Var m a Float -> Var m a (b, Transform) -> Var m a (b, Transform)
rotated vRot vEl = proc a -> do
    l <- vEl  -< a
    r <- vRot -< a
    returnA -< rotate r <$> l
--------------------------------------------------------------------------------
-- UITree UIElement streams
--------------------------------------------------------------------------------
uinetwork :: (TimeDelta r)
          => Vareff r InputEvent [(Element, Transform)]
uinetwork = sequenceA [ mainButton ]

mainButton :: (TimeDelta r) => Vareff r InputEvent (Element, Transform)
mainButton =
    (,) <$> (Element <$> btn') <*> t
    where btn' = btn `orE` btnDown
          btnDown = (btn ~> clr) `tagOn` curs
          curs = cursorInside poly
          poly = transformPoly <$> t <*> pure p
          clr = var $ \(Button b l) -> Button (b{boxColor = V4 0.3 0.3 0.3 1}) l
          btn = button sz "Button" "Arial Black" (PointSize 32)
                                                 (V4 0 0 0 1)
                                                 (V4 1 1 1 1)
          sz = br
          p@[_, _, br, _] = [zero, V2 122 0, V2 122 40, V2 0 40]
          t  = time ~> (Transform <$> (V2 <$> easex <*> easey) <*> pure (V2 1 1) <*> easer)
          there = tween easeOutExpo 0 100 2
          back  = tween easeOutExpo 100 0 2
          waitThere = constant 100 2
          waitHere = constant 0 2
          easex = there `andThenE` waitThere `andThenE` back `andThenE` waitHere `andThen` easex
          easey = waitHere `andThenE` there `andThenE` waitThere `andThenE` back `andThen` easey
          easer = tween linear 0 (2*pi) 1 `andThenE` constant 0 1 `andThen` easer


label :: String -> String -> PointSize -> Color
      -> Vareff r InputEvent Label
label str fn ps clr = pure l
    where l = Label str fn ps clr

box :: Size -> Color -> Vareff r InputEvent Box
box sz clr = Box <$> pure sz <*> pure clr

button :: Size -> String -> String -> PointSize -> Color -> Color
       -> Vareff r InputEvent Button
button sz str fn ps bclr fclr = Button <$> bx <*> lb
    where bx = box sz bclr
          lb = label str fn ps fclr

