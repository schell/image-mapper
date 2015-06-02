{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
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
import Control.Eff.Fresh
import Data.Bits
import Data.Typeable
import Data.Monoid
import Data.Text (pack)
import System.Exit
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

--newtype DetachedRenderers = Detached { detached :: IntMap (UIElement, Renderer) }
newtype AttachedRenderers = Attached { attached :: IntMap (UIElement, Renderer) }

data Rez = Rez { rezGeom      :: GeomRenderSource
               , rezBez       :: BezRenderSource
               , rezWindow    :: Window
               , rezFontCache :: Async FontCache
               } deriving (Typeable)

type MakesScene r = ( Member (Reader Rez) r
                    , Member (Fresh Uid) r
                    , Member (State AttachedRenderers) r
                    , SetMember Lift (Lift IO) r
                    )

stepScene :: MakesScene r => [UITree UIElement] -> Eff r ()
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

treeTransform :: UITree a -> Transform
treeTransform (UIBranch _ t _) = t
treeTransform (UILeaf _ t _) = t

attach :: (Member (State AttachedRenderers) r, SetMember Lift (Lift IO) r)
       => Uid -> (UIElement, Renderer) -> Eff r ()
attach i a' = do
    mAR <- lookupAttached i
    case mAR of
        Nothing -> return ()
        Just ar -> lift $ rCleanup $ snd ar
    modify $ \(Attached rs) -> Attached $ IM.insert (unUid i) a' rs

lookupAttached :: (Member (State AttachedRenderers) r)
               => Uid -> Eff r (Maybe (UIElement, Renderer))
lookupAttached (Uid i) = (IM.lookup i . attached) <$> get

newRenderer :: MakesScene r => Uid -> UIElement -> Eff r Renderer
newRenderer i el = do
    r <- uiRenderer el
    attach i (el, r)
    return r

renderUI :: MakesScene r => UITree UIElement -> Eff r ()
renderUI = renderTree mempty

renderTree :: MakesScene r => Transform -> UITree UIElement -> Eff r ()
renderTree t (UIBranch _ t' ts) = mapM_ (renderTree $ t <> t') ts
renderTree t (UILeaf i t' el') = do
    mAR <- lookupAttached i
    r <- case mAR of
        Nothing -> newRenderer i el'
        Just (el, r) -> if el' == el
                        then return r
                        else newRenderer i el'
    lift $ (rRender r) $ t <> t'

uiRenderer :: MakesScene r => UIElement -> Eff r Renderer
uiRenderer (UIBox (AABB (V2 x y) (V2 hw hh)) c) = do
    lift $ putStrLn "Compiling a box renderer."
    Rez geom _ w _ <- ask
    let [tl, tr, br, bl] = [V2 (x-hw) (y-hh), V2 (x+hw) (y-hh), V2 (x+hw) (y+hh), V2 (x-hw) (y+hh)]
        vs = [tl, tr, br, tl, br, bl]
        cs = replicate 6 c
    lift $ colorRenderer w geom GL_TRIANGLES vs cs

uiRenderer (UILabel fn str ps _ bc) = do
    lift $ putStrLn "Compiling a label renderer."
    Rez geom _ w afc <- ask
    dpi <- lift $ calculateDpi
    let desc = FontDescriptor (pack fn) $ FontStyle False False
    lift $ withAsyncRenderer afc $ \fc -> do
        mRend <- withFont fc desc $ \font -> do
            let BoundingBox{..} = stringBoundingBox font dpi ps str
                [tl, tr, br, bl] = [V2 _xMin _yMin, V2 _xMax _yMin, V2 _xMax _yMax, V2 _xMin _yMax]
                vs = [tl, tr, br, tl, br, bl]
                cs = replicate 6 bc
            background <- colorRenderer w geom GL_TRIANGLES vs cs
            return background
        case mRend of
            Nothing -> return mempty
            Just r  -> return r

