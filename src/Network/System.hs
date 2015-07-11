{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Network.System where

import Types.Internal
import Prelude hiding (sequence_)
import Linear
import Graphics.Text.TrueType
import Graphics.UI.GLFW hiding (Image(..))
import Gelatin.Core.Triangulation.Common
import Control.Concurrent.Async
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Control.Varying
import Control.Varying.Time
import Control.Arrow
import Data.Time.Clock
import Data.Text (pack)

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------
mouseScrolled :: Monad m => Var m InputEvent (Event (V2 Float))
mouseScrolled = arr check ~> onJust
    where check (ScrollEvent x y) = Just $ fmap realToFrac $ V2 x y
          check _ = Nothing

windowResized :: Monad m => Var m InputEvent (Event (Int, Int))
windowResized = arr check ~> onJust
    where check (WindowSizeEvent w h) = Just (w,h)
          check _ = Nothing

mouseButtonDragged :: Monad m => MouseButton -> Var m InputEvent (Event Vector)
mouseButtonDragged btn =
    latchWith (-) dragPos $ takeE 1 dragPos `andThenE` delay 0 dragPos
    where dragPos  = latchWith const cursorMoved dragging
          dragging = between down up
          down     = mouseButtonPressed btn
          up       = mouseButtonReleased btn

cursorMoved :: Monad m => Var m InputEvent (Event Position)
cursorMoved = arr check ~> onJust
    where check (CursorMoveEvent x y) = Just $ realToFrac <$> V2 x y
          check _ = Nothing

mouseButtonPressed :: Monad m => MouseButton -> Var m InputEvent (Event Position)
mouseButtonPressed btn = latchWith const cursorMoved (filterE check mouseDown)
    where check = (== btn) . fst

mouseButtonReleased :: Monad m => MouseButton -> Var m InputEvent (Event Position)
mouseButtonReleased btn = latchWith const cursorMoved (filterE check mouseUp)
    where check = (== btn) . fst

mouseDown :: Monad m => Var m InputEvent (Event (MouseButton, ModifierKeys))
mouseDown = mouseAction ~> arr (check . toMaybe) ~> onJust
    where check (Just (mb, MouseButtonState'Pressed, mks)) = Just (mb, mks)
          check _ = Nothing

mouseUp :: Monad m => Var m InputEvent (Event (MouseButton, ModifierKeys))
mouseUp = mouseAction ~> arr (check . toMaybe) ~> onJust
    where check (Just (mb, MouseButtonState'Released, mks)) = Just (mb, mks)
          check _ = Nothing

mouseAction :: Monad m => Var m InputEvent (Event (MouseButton, MouseButtonState, ModifierKeys))
mouseAction = arr check ~> onJust
    where check (MouseButtonEvent mb mbs mks) = Just (mb,mbs,mks)
          check _ = Nothing

cursorInside :: Monad m => Var m InputEvent Poly -> Var m InputEvent (Event ())
cursorInside vpoly = proc e -> do
    poly <- vpoly -< e
    pos  <- cursorStartingAt 0 -< e
    onTrue -< pointInside pos poly

filesDropped :: Monad m => Var m InputEvent (Event [String])
filesDropped = arr check ~> onJust
    where check (FileDropEvent path) = Just path
          check _ = Nothing

fileDropped :: Monad m => Var m InputEvent (Event String)
fileDropped = (head <$>) <$> filesDropped
--------------------------------------------------------------------------------
-- Continuous Streams
--------------------------------------------------------------------------------
cursorStartingAt :: Monad m => Position -> Var m InputEvent Position
cursorStartingAt = (cursorMoved ~>) . startingWith

windowSize :: (ReadsRez r, DoesIO r)
           => Vareff r InputEvent (V2 Float)
windowSize = Var $ \_ -> do
    win <- rezWindow <$> ask
    (w,h) <- lift $ getWindowSize win
    let v  = fmap realToFrac $ V2 w h
        resized = (fmap realToFrac . uncurry V2 <$>) <$> windowResized
    return (v, resized ~> startingWith v)

--time :: (TimeDelta r, RealFrac b) => Vareff r a b
--time = Var $ \_ -> do
--   Delta dt <- get
--   return (realToFrac dt, time)

time :: (DoesIO r, RealFrac b) => Vareff r a b
time = delta (lift $ getCurrentTime) (\a b -> realToFrac $ diffUTCTime a b)

systemFontCache :: (ReadsRez r, DoesIO r) => Vareff r a (Maybe FontCache)
systemFontCache = Var $ \_ -> do
    afc  <- rezFontCache <$> ask
    mefc <- lift $ poll afc
    case mefc of
        Just (Right fc) -> return (Just fc, pure $ Just fc)
        Just (Left _)   -> return (Nothing, pure Nothing)
        _               -> return (Nothing, systemFontCache)

systemPathForFont :: (ReadsRez r, DoesIO r)
                  => String -> Bool -> Bool -> Vareff r a (Maybe FilePath)
systemPathForFont f b i = systemFontCache ~> (var $ findFont f b i)
    where findFont fam bold italic (Just fc) = findFontInCache fc $
              FontDescriptor (pack fam) $ FontStyle bold italic
          findFont _ _ _ _ = Nothing
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
(#) :: a -> (a -> b) -> b
(#) = flip ($)
infixl 0 #
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

