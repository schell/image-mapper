{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Network (network, Network(..), module M) where

import Types.Internal
import Types.Renderable
import UI.Label
import Network.MappingScreen as M
import Prelude hiding (sequence_)
import Linear
import Network.System
import Gelatin.Core.Rendering
import Gelatin.Core.Color
import Graphics.Text.TrueType
import Control.Varying
import Control.Eff
import Control.Eff.State.Strict
import Data.Hashable
import Data.Typeable
import qualified Data.IntMap as IM
import Data.List

network :: (DoesIO r, ReadsRez r, Member (State AttachedRenderings) r)
        => Vareff r InputEvent Element
network = Element <$>
    (Network <$> (mappingScreen ~> var tfrm)
             <*> breadCrumb
             <*> debugInfo)
    where tfrm m = m{ mappingScreenTfrm = Transform (V2 0 16) 1 0 }

breadCrumb :: (ReadsRez r, DoesIO r) => Vareff r InputEvent Label
breadCrumb = Label <$> (pure $ Transform (V2 10 26) 1 0)
                   <*> (intercalate " > " <$> strs)
                   <*> systemPathForFont "Arial" True False
                   <*> (pure $ PointSize 14)
                   <*> pure white
    where strs = (:) <$> (show <$> networkMode) <*> subModeStrings

networkMode :: Monad m => Var m a NetworkMode
networkMode = pure NetworkModeMappingScreen

subModeStrings :: Monad m => Var m InputEvent [String]
subModeStrings = sequenceA [show <$> mappingScreenMode]

debugInfo :: (Member (State AttachedRenderings) r, DoesIO r, ReadsRez r)
          => Vareff r InputEvent Label
debugInfo = Label <$> tfrm
                  <*> str
                  <*> systemPathForFont "Arial" False False
                  <*> (pure $ PointSize 12)
                  <*> pure white
    where tfrm :: (DoesIO r, ReadsRez r) => Vareff r InputEvent Transform
          tfrm = Transform <$> (V2 <$> 0 <*> y)
                           <*> 1 <*> 0
          str :: Member (State AttachedRenderings) r => Vareff r a String
          str = (("Renderings: " ++) . show) <$> numRenderings
          y :: (ReadsRez r, DoesIO r) => Vareff r InputEvent Float
          y = (\(V2 _ y') -> y' - 8) <$> windowSize

--x :: DoesIO r => Vareff r a Float
--x = time ~> tween easeOutExpo 0 500 1 `andThenE` tween easeOutExpo 500 0 1 `andThen` x

numRenderings :: (Member (State AttachedRenderings) r) => Vareff r a Int
numRenderings = varM $ \_ -> do
    Attached rms <- get
    return $ IM.size rms
--------------------------------------------------------------------------------
-- Element streams
--------------------------------------------------------------------------------
--btnGraph :: Monad m => ButtonGraph m
--btnGraph = ButtonGraph btn poly st
--    where tfrm :: Monad m => Var m a Transform
--          tfrm = pure mempty
--
--          poly :: Monad m => Var m a Poly
--          poly = transformPoly <$> tfrm <*> pure [V2 0 0, V2 100 0, V2 100 50, V2 0 50]
--
--          mrgst eIn eDwn = (const $ const ButtonStateOn) <$> eIn <*> eDwn
--                           <|> ButtonStateHover <$ eIn
--          st :: Monad m => Var m InputEvent ButtonState
--          st = pure ButtonStateOff `orE` (mrgst <$> cursorInside poly
--                                                <*> between mouseDown mouseUp)
--
--          btn :: Monad m => Var m InputEvent Button
--          btn = Button <$> pure mempty <*> bx <*> lbl
--
--          bx :: Monad m => Var m InputEvent Box
--          bx = Box <$> pure mempty <*> (sz <$> poly) <*> (stateColor <$> st)
--
--          sz [_, _, s, _] = s
--          sz _ = 0
--
--          stateColor ButtonStateOff   = 0.3
--          stateColor ButtonStateHover = 0.4
--          stateColor ButtonStateOn = 0.6
--
--          lbl :: Monad m => Var m a Label
--          lbl = Label <$> pure mempty <*> pure "Save" <*> pure "Arial" <*> (pure $ PointSize 32) <*> 1


instance Renderable Network where
    nameOf _ = "Network"
    children (Network m bc i) = [Element m, Element i, Element $ shadow bc, Element bc]
    cache = cacheChildren
    hashes n@(Network m bc i) = hash n : concat [ hashes bc
                                                , hashes $ shadow bc
                                                , hashes m
                                                , hashes i
                                                ]

    transformOf _ = mempty


shadow bc = bc{ labelTransform = translate 0 1 $ labelTransform bc
              , labelColor = black `alpha` 0.5
              }

instance Hashable Network where
   hashWithSalt s (Network m bc l) =
       s `hashWithSalt` bc `hashWithSalt` m `hashWithSalt` l

data Network = Network { networkMappingScreen :: MappingScreen
                       , networkBreadCrumb    :: Label
                       , networkInfo          :: Label
                       } deriving (Show, Eq, Typeable)

instance Show NetworkMode where
    show NetworkModeMappingScreen = "Mapping Screen"

data NetworkMode = NetworkModeMappingScreen

