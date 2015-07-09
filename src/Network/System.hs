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
import Control.Eff.State.Strict
import Control.Arrow
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

windowSize :: (ReadsRez r, DoesIO r)
           => Vareff r InputEvent (V2 Float)
windowSize = Var $ \_ -> do
    win <- rezWindow <$> ask
    (w,h) <- lift $ getWindowSize win
    let v  = fmap realToFrac $ V2 w h
        resized = (fmap realToFrac . uncurry V2 <$>) <$> windowResized
    return (v, resized ~> startingWith v)

time :: (TimeDelta r, RealFrac a) => Vareff r b a
time = realToFrac <$> delta (unDelta <$> get) (-)

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

