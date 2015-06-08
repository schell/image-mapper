{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Network where

import UI.Types
import Scene
import Prelude hiding (sequence_)
import Gelatin.Core.Render hiding (scaled, trace)
import Gelatin.Core.Triangulation.Common
import Control.Varying
import Control.Varying.Time
import Control.Eff.State.Strict
import Control.Arrow
import Control.Applicative
import Debug.Trace

data ButtonState = ButtonStateOff | ButtonStateHover | ButtonStateOn
data ButtonGraph m = ButtonGraph { buttonGraphBtn   :: Var m InputEvent Button
                                 , buttonGraphPoly  :: Var m InputEvent Poly
                                 , buttonGraphState :: Var m InputEvent ButtonState
                                 }

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
--------------------------------------------------------------------------------
-- UITree UIElement streams
--------------------------------------------------------------------------------
uinetwork :: (TimeDelta r)
          => Vareff r InputEvent [Element]
uinetwork = sequenceA [ mainButton ]

mainButton :: (TimeDelta r) => Vareff r InputEvent Element
mainButton = Element <$> btn
    where ButtonGraph btn _ _ = btnGraph

btnGraph :: Monad m => ButtonGraph m
btnGraph = ButtonGraph btn poly st
    where tfrm :: Monad m => Var m a Transform
          tfrm = pure mempty

          poly :: Monad m => Var m a Poly
          poly = transformPoly <$> tfrm <*> pure [V2 0 0, V2 100 0, V2 100 50, V2 0 50]

          mrgst eIn eDwn = (const $ const ButtonStateOn) <$> eIn <*> eDwn
                           <|> ButtonStateHover <$ eIn
          st :: Monad m => Var m InputEvent ButtonState
          st = pure ButtonStateOff `orE` (mrgst <$> cursorInside poly <*> (between mouseDown mouseUp ~> (var $ \s -> trace (show s) s)))

          btn :: Monad m => Var m InputEvent Button
          btn = Button <$> pure mempty <*> bx <*> lbl

          bx :: Monad m => Var m InputEvent Box
          bx = Box <$> pure mempty <*> (sz <$> poly) <*> (stateColor <$> st)

          sz [_, _, s, _] = s
          sz _ = 0

          stateColor ButtonStateOff   = 0.3
          stateColor ButtonStateHover = 0.4
          stateColor ButtonStateOn = 0.6

          lbl :: Monad m => Var m a Label
          lbl = Label <$> pure mempty <*> pure "Button" <*> pure "Arial" <*> (pure $ PointSize 32) <*> 1
