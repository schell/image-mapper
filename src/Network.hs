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

type Vareff r = Var (Eff r)

maybeCursorMoved :: Monad m => Var m InputEvent (Maybe (Double, Double))
maybeCursorMoved = arr check
    where check (CursorMoveEvent x y) = Just (x,y)
          check _ = Nothing

cursorMoved :: Monad m => Var m InputEvent (Event (Double, Double))
cursorMoved = maybeCursorMoved ~> onJust

cursorStartingAt :: Monad m => (Double, Double) -> Var m InputEvent (Double, Double)
cursorStartingAt = (cursorMoved ~>) . startingWith

uid :: Member (Fresh Uid) r => Eff r Uid
uid = fresh

text :: (Member (Reader Rez) r,
         SetMember Lift (Lift IO) r)
      => String -> Vareff r a Scene
text str = Var $ \_ -> do
    afc <- rezFontCache <$> ask
    mefc <- lift $ poll afc
    case mefc of
        Nothing -> return (mempty, text str)
        Just efc -> case efc of
                        Left _ -> return (mempty, pure mempty)
                        Right fc -> return (mempty, textWithFontCache fc str)

box :: (Member (Reader Rez) r,
        SetMember Lift (Lift IO) r)
    => AABB -> V4 Float -> Eff r Scene
box (AABB (V2 x y) (V2 hw hh)) c = do
    Rez grs brs win _ <- ask
    let tl = V2 (x-hw) (y-hh)
        tr = V2 (x+hw) (y-hh)
        bl = V2 (x-hw) (y+hh)
        br = V2 (x+hw) (y+hh)
        vs = [tl,tr,bl
             ,tl,bl,br
             ]
        cs = replicate (length vs) c
    r <- lift $ colorRenderer win grs GL_TRIANGLES vs $ cs
    return $ Scene mempty r

rezWhile :: MakesScene r => Scene -> Vareff r Bool (Event Scene)
rezWhile s = event `andThenE` rez
    where event = use s onTrue
          rez = Var $ \_ -> destroyScene >> return (NoEvent, pure NoEvent)

button :: MakesScene r => String -> AABB -> Vareff r a Scene
button txt aabb = undefined --Var $ \_ _ -> do
    --r <- lift $ do

textWithFontCache :: (Member (Reader Rez) r,
                      SetMember Lift (Lift IO) r)
                  => FontCache -> String -> Vareff r a Scene
textWithFontCache fc str = Var $ \_ -> do
    Rez grs brs win _ <- ask
    dpi <- lift $ calculateDpi
    mtextR <- lift $ withFont fc (FontDescriptor "Arial" $ FontStyle False False) $ \font -> do
        textR <- colorFontRenderer win grs brs dpi
                                   (FontString font 32 str)
                                   (\(V2 x _) ->
                                       lerp (x/100) (V4 1 1 0 1) (V4 1 0 1 1))
        return textR
    case mtextR of
        Nothing -> return (mempty, pure mempty)
        Just textR -> let scene = Scene mempty textR
                      in return (scene, pure scene)

time :: (SetMember Lift (Lift IO) r, Fractional t) => Vareff r b t
time = delta (lift $ getCurrentTime)
             (\a b -> realToFrac $ diffUTCTime a b)

network :: (Member (Reader Rez) r,
            Member (Fresh Uid) r,
            SetMember Lift (Lift IO) r)
        => Vareff r InputEvent Scene
network =
    SceneContainer <$> (time ~> easing)
                   <*> sequenceA [text "Syndeca Mapper"{-, button "button" $ AABB (V2 100 100) (V2 50 25)-}]
        where easing :: DoesIO r => Vareff r Double Transform
              easing = liftA2 (\x y -> translate (realToFrac x) (realToFrac y) mempty)
                              ease
                              ease
              ease :: DoesIO r => Vareff r Double Double
              ease = to `andThenE` from `andThen` ease
              linearTo :: SetMember Lift (Lift IO) r
                       => Double -> Double -> Double
                       -> Vareff r Double (Event Double)
              linearTo f t d = tween linear f t d

              to :: DoesIO r => Vareff r Double (Event Double)
              to = tween easeOutExpo 0 500 1

              from :: DoesIO r => Vareff r Double (Event Double)
              from = tween easeOutExpo 500 0 1
