{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Network where

import UI.Types
import Network.System
import Network.MappingScreen
import Scene
import System
import Prelude hiding (sequence_)
import Linear
import Graphics.UI.GLFW hiding (Image(..))
import Graphics.Text.TrueType
import Gelatin.Core.Render
import Gelatin.Core.Triangulation.Common
import Control.Eff
import Control.Eff.Lift
import Control.Varying
import Control.Varying.Time
import Control.Eff.State.Strict
import Control.Arrow
import Control.Applicative

--------------------------------------------------------------------------------
-- Element streams
--------------------------------------------------------------------------------
uinetwork :: (TimeDelta r, MakesScene r)
          => Vareff r InputEvent [Element]
uinetwork = sequenceA [ mainButton, mappingScreen ]

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
          st = pure ButtonStateOff `orE` (mrgst <$> cursorInside poly
                                                <*> between mouseDown mouseUp)

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
          lbl = Label <$> pure mempty <*> pure "Save" <*> pure "Arial" <*> (pure $ PointSize 32) <*> 1
