{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Scene where

import Prelude hiding (sequence_)
import UI
import Gelatin.Core.Render hiding (el, trace)
import Control.Concurrent.Async
import Graphics.GL.Core33
import Control.Concurrent
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Data.Hashable
import Data.Bits
import Data.Typeable
import Data.Monoid
import Data.List (intercalate)
import Data.Text (pack)
import System.Exit
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

newtype AttachedRenderers = Attached { attached :: IntMap Renderer }

data Rez = Rez { rezGeom      :: GeomRenderSource
               , rezBez       :: BezRenderSource
               , rezWindow    :: Window
               , rezFontCache :: Async FontCache
               } deriving (Typeable)

type MakesScene r = ( Member (Reader Rez) r
                    , Member (State AttachedRenderers) r
                    , SetMember Lift (Lift IO) r
                    )

type ModifiesRenderers r = (Member (State AttachedRenderers) r
                           ,SetMember Lift (Lift IO) r
                           )

class Renderable a where
    transformOf :: a -> Transform
    render    :: MakesScene r => a -> Eff r (Maybe Renderer)

instance Renderable Box where
    render (Box _ sz c) = uiBox sz c >>= return . Just
    transformOf = boxTransform

instance Renderable Label where
    render (Label _ str fn ps fc) = do
        afc <- rezFontCache <$> ask
        mefc <- lift $ poll afc
        case mefc of
            Just (Right cache) -> uiLabel cache str fn ps fc >>= return . Just
            _                  -> return Nothing
    transformOf = labelTransform

instance Renderable Button where
    render (Button t box label) = do
        mb <- render box
        ml <- render label
        return $ do b <- mb
                    l <- ml
                    return $ (transformRenderer t b) <> (transformRenderer t l)
    transformOf = buttonTransform

data Element where
    Element  :: (Renderable a, Hashable a) => a -> Element

stepScene :: MakesScene r => [Element] -> Eff r ()
stepScene els = do
    win <- rezWindow <$> ask
    lift $ do
        (fbw,fbh) <- getFramebufferSize win
        glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

    mapM_ renderUI els

    lift $ do
        pollEvents
        swapBuffers win
        shouldClose <- windowShouldClose win
        if shouldClose
        then exitSuccess
        else threadDelay 100

attach :: (Member (State AttachedRenderers) r, SetMember Lift (Lift IO) r)
       => Int -> Renderer -> Eff r ()
attach i a' = do
    mR <- lookupAttached i
    case mR of
        Nothing -> return ()
        Just r -> lift $ rCleanup r
    modify $ \(Attached rs) -> Attached $ IM.insert i a' rs

lookupAttached :: (Member (State AttachedRenderers) r)
               => Int -> Eff r (Maybe Renderer)
lookupAttached i = (IM.lookup i . attached) <$> get

-- | Create a new renderer for the UIElement and attach it with the given
-- Uid. If the renderer cannot be created return a no-op and attach
-- nothing.
newRenderer :: (Renderable a, MakesScene r, Hashable a)
            => Int -> a -> Eff r Renderer
newRenderer i el = do
    mr <- render el
    r <- case mr of
        Just r  -> attach i r >> return r
        Nothing -> return mempty
    return r

renderUI :: (MakesScene r) => Element -> Eff r ()
renderUI (Element a) = do
    let h' = hash a
    mAR <- lookupAttached h'
    -- | TODO: Deal with the old unused renderers.
    r <- case mAR of
        Nothing -> newRenderer h' a
        Just r -> return r
    lift $ (rRender r) (transformOf a)

uiLabel :: MakesScene r
        => FontCache -> String -> String -> PointSize -> V4 Float
        -> Eff r Renderer
uiLabel cache str fn ps fc = do
    Rez geom bez w _ <- ask
    dpi <- lift $ calculateDpi
    let desc = FontDescriptor (pack fn) $ FontStyle False False
    mRend <- lift $ withFont cache desc $ \font -> do
        -- | TODO: figure out why the bounding box is too low
        let BoundingBox{..} = stringBoundingBox font dpi ps str
            height = _yMax - _yMin
            fs = FontString font (getPointSize ps) (0, height) str
        colorFontRenderer w geom bez dpi fs (const fc)
    case mRend of
        Nothing -> do lift $ do putStrLn $ "Could not find " ++ show desc
                                let fnts = enumerateFonts cache
                                    fstr = intercalate "\n    " $ map show fnts
                                putStrLn $ "Available:\n" ++ fstr
                      return mempty
        Just r  -> return r

uiBox :: MakesScene r => Size -> V4 Float -> Eff r Renderer
uiBox (V2 w h) c = do
    Rez geom _ win _ <- ask
    let [tl, tr, br, bl] = [zero, V2 w 0, V2 w h, V2 0 h]
        vs = [tl, tr, br, tl, br, bl]
        cs = replicate 6 c
    lift $ colorRenderer win geom GL_TRIANGLES vs cs
