{-# LANGUAGE FlexibleContexts #-}
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
import Gelatin.Core.Render
import Gelatin.Core.Color
import Graphics.Text.TrueType
import Control.Varying
import Control.Eff.State.Strict
import Data.Hashable
import Data.Typeable
import Data.Monoid
import qualified Data.IntMap as IM

network :: (MakesScene r)
        => Vareff r InputEvent Network
network = Network <$> mappingScreen
                  <*> debugInfo
                  <*> (mappingScreenRequests <$> mappingScreen)

debugInfo :: MakesScene r => Vareff r InputEvent Label
debugInfo = Label <$> tfrm
                  <*> str
                  <*> systemPathForFont "Arial" False False
                  <*> (pure $ PointSize 12)
                  <*> pure white
    where tfrm :: (MakesScene r) => Vareff r InputEvent Transform
          tfrm = Transform <$> ((\(V2 _ y) -> V2 0 y) <$> windowSize)
                           <*> 1 <*> 0
          str :: MakesScene r => Vareff r a String
          str = (("Renderers: " ++) . show) <$> numRenderers

numRenderers :: MakesScene r => Vareff r a Int
numRenderers = varM $ \_ -> do
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
    render n@(Network ms i _) = do
        Renderer fms _ <- getRenderer ms
        Renderer fi _  <- getRenderer i
        let f t = do fms $ t <> transformOf ms
                     fi $ t <> transformOf i
            c = putStrLn $ "Cleaning a network " ++ (show $ hash n)
        return $ Just $ Renderer f c

    renderingHashes n@(Network m i _) =
        hash n : (renderingHashes m) ++ (renderingHashes i)

    transformOf _ = mempty

instance Hashable Network where
   hashWithSalt s (Network m l _) = s `hashWithSalt` m `hashWithSalt` l

data Network = Network { networkMappingScreen :: MappingScreen
                       , networkInfo          :: Label
                       , networkRequests      :: [Request]
                       } deriving (Show, Eq, Typeable)
